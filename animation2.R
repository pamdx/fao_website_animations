rm(list = ls())

library(dplyr)
library(tidyr)
library(sf)
library(gganimate)
library(showtext)

font_add_google("Montserrat") # Load FAO font
showtext_auto()

source("functions.R")

# Parameters

species <- c("Fish, crustaceans and molluscs, etc.") # choices : "Fish, crustaceans and molluscs, etc.", "Aquatic plants"
source <- "Capture production"
source_title <- switch(source, "Aquaculture production" = "Aquaculture production", "Capture production" = "Capture fisheries production")
area <- c("Inland waters") # choices: "Inland waters" "Marine areas"
format <- "landscape"
framerate <- 30
color_inland <- ifelse(source == "Capture production", "#0091a5", "#FFCC31")
color_marine <- ifelse(source == "Capture production", "#7babc0", "#f6800d")

# Import data

centroids <- read_sf('./inputs/FAO_fishing_areas_shp/FAO_fishing_areas_shp.shp') %>% select(F_CODE, Lat, Lon) %>%
  add_row(F_CODE = 7, Lat = 7830554, Lon = 6484471) %>% # add missing coordinate for USSR inland areas
  mutate(Lat = ifelse(F_CODE == 5, 365210.2, Lat), Lon = ifelse(F_CODE == 5, 5378969, Lon)) %>% # relocate Europe's bubble to western Europe to avoid confusion with the USSR bubble
  st_drop_geometry()

data_fishing_area_yearly <- readRDS("inputs/data.RDS") %>%
  filter(species_group %in% species, production_source_name == source, inland_marine_group_en %in% area) %>%
  group_by(production_source_name, fishing_area_code, fishing_area_name, unit, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(size_bubbles = sqrt(value/max(value)*60)) %>%
  mutate(size_text = value/max(value)*15) %>%
  complete(year, nesting(production_source_name, fishing_area_code, fishing_area_name, unit), fill = list(value = 0)) %>% # This is needed to avoid weird easing effects when the number of groups per year isn't constant throughout the years
  separate(fishing_area_name, c("fishing_area_group", "fishing_area_subgroup"), ", ", remove = FALSE) %>% # Much clearer result when grouping by ocean
  group_by(year) %>%
  mutate(total = sum(value)) %>%
  mutate(percentage = value/total) %>%
  mutate(annotation_map = ifelse(percentage >= 0.01, scales::label_percent(accuracy = 1)(percentage), "")) %>%
  left_join(centroids, by = c("fishing_area_code" = "F_CODE")) %>%
  st_as_sf(coords = c("Lat", "Lon"), crs = "ESRI:54030", remove = FALSE)

data_fishing_area_decadal <- readRDS("inputs/data.RDS") %>%
  filter(species_group %in% species, production_source_name == source, inland_marine_group_en %in% area) %>%
  group_by(production_source_name, fishing_area_code, fishing_area_name, unit, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(decade = as.integer(year - year %% 10)) %>%
  mutate(label = paste0(decade, "s")) %>%
  group_by(production_source_name, fishing_area_code, fishing_area_name, unit, decade, label) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  complete(decade, nesting(production_source_name, fishing_area_code, fishing_area_name, unit), fill = list(value = 0)) %>% # This is needed to avoid weird easing effects when the number of groups per year isn't constant throughout the years
  separate(fishing_area_name, c("fishing_area_group", "fishing_area_subgroup"), ", ", remove = FALSE) %>% # Much clearer result when grouping by ocean
  group_by(decade) %>%
  mutate(total = sum(value)) %>%
  mutate(percentage = value/total) %>%
  mutate(annotation_map = ifelse(percentage >= 0.01, scales::label_percent(accuracy = 1)(percentage), "")) %>%
  ungroup() %>%
  mutate(size_bubbles = sqrt(value)/sqrt(max(value))*40) %>%
  mutate(size_text = value/max(value)*15) %>%
  left_join(centroids, by = c("fishing_area_code" = "F_CODE")) %>%
  st_as_sf(coords = c("Lat", "Lon"), crs = "ESRI:54030", remove = FALSE)

# Load shapefile

fishing_areas_shp <- read_sf('./inputs/FAO_fishing_areas_shp/FAO_fishing_areas_shp.shp') %>%
  st_transform("ESRI:54030") %>% # Reproject as Robinson
  st_simplify(preserveTopology = FALSE, dTolerance = 20000) %>% # Simplify polygons
  select(-c(Lat, Lon)) %>%
  mutate(water_type = ifelse(water_type == "Marine waters", "Marine areas", water_type)) %>%
  filter(water_type %in% area)

# Plot map  

if (format == "landscape") {
 
  p <- ggplot() +
    geom_sf(data = fishing_areas_shp, fill = "white", color = "darkgray", alpha = 0.7, size = 0.15) +
    geom_sf(data = data_fishing_area_yearly %>% filter(value != 0), aes(geometry = geometry, group = fishing_area_code, color = fishing_area_code < 10, size = value), alpha = 0.85) +
    geom_text(data = data_fishing_area_yearly, aes(x = Lat, y = Lon, label = annotation_map), color = "white", alpha = 0.85, fontface = "bold", show.legend = FALSE) +
    scale_size_area(
      max_size = 50,
      breaks = scales::breaks_log(n = 6, base = 10)(1000:max(data_fishing_area_yearly$value)),
      labels = addUnits,
      name = "Production") +
    # geom_text(data = data_fishing_area_yearly, aes(label = as.factor(year)), x = 15000000, y = -8400000, alpha = 0.2,  col = "gray", size = 20) +
    scale_colour_manual(name = 'Water type', values = setNames(c(color_inland,color_marine), c(T, F))) +
    theme_void() + 
    guides(color = "none", size = guide_legend(override.aes = list(color = ifelse(length(area) > 1, "gray", switch(area, "Marine areas" = color_marine, "Inland waters" = color_inland))))) +
    theme(legend.position = "bottom") +
    labs(title = paste0(source_title, ifelse(source == "Capture production", " by FAO major fishing area", " by FAO major area")),
         subtitle = paste0(ifelse(length(area) > 1, "", paste0(area, ", ")), paste(range(data_fishing_area_yearly$year), collapse = "-"), ', year: {frame_time}'), 
         caption = ifelse(length(species) > 1, "The percentages refer to the area's share of global aquaculture production. \n The data include aquatic animals and algae. Units: tonnes - live weight (animals), wet weight (algae).", "The percentages refer to the fishing area's share of global capture fisheries production. The data only include aquatic animals. Units: tonnes - live weight.")) +
    # gganimate part
    # labs(subtitle = 'Year: {frame_time}') +
    transition_time(year) +
    ease_aes('linear') +
    enter_fade() +
    exit_shrink() 
  
} else{
  
  p <- ggplot() +
    geom_sf(data = fishing_areas_shp, fill = "white", color = "darkgray", alpha = 0.7, size = 0.15) +
    geom_sf(data = data_fishing_area_decadal %>% filter(value != 0), aes(geometry = geometry, group = fishing_area_code, color = fishing_area_code < 10, size = value), alpha = 0.85) +
    geom_text(data = data_fishing_area_decadal, aes(x = Lat, y = Lon, label = annotation_map), color = "white", alpha = 0.85, fontface = "bold", show.legend = FALSE) +
    scale_size_area(
      max_size = 40,
      breaks = scales::breaks_log(n = 6, base = 10)(1000:max(data_fishing_area_decadal$value)), 
      labels = addUnits, 
      name = "Production") +
    # geom_text(data = data_fishing_area_decadal, aes(label = as.factor(label)), x = 15000000, y = -8400000, alpha = 0.2,  col = "gray", size = 20) +
    scale_colour_manual(name = 'Water type', values = setNames(c(color_inland,color_marine), c(T, F))) +
    theme_void() + 
    guides(color = "none", size = guide_legend(override.aes = list(color = ifelse(length(area) > 1, "gray", switch(area, "Marine areas" = color_marine, "Inland waters" = color_inland))))) +
    theme(legend.position = "bottom") +
    labs(title = paste0(source_title, ifelse(source == "Capture production", " by FAO major fishing area", " by FAO major area")),
         subtitle = stringr::str_to_sentence(paste0(ifelse(length(area) > 1, "", paste0(area, ", ")), "decade average, ", paste(range(data_fishing_area_yearly$year), collapse = "-"), paste0(', decade: {closest_state}', 's'))),
         caption = ifelse(length(species) > 1, "The percentages refer to the area's share of global aquaculture production. \n The data include aquatic animals and algae. Units: tonnes - live weight (animals), wet weight (algae).", "The percentages refer to the fishing area's share of global capture fisheries production. The data only include aquatic animals. Units: tonnes - live weight.")) +
    # gganimate part
    # labs(subtitle = paste0('Decade: {closest_state}', 's')) +
    transition_states(decade, transition_length = 1, state_length = 0, wrap = FALSE) +
    ease_aes('linear') +
    enter_fade() +
    exit_shrink()
  
}

# Render animation

future::plan("multisession", workers = 8)
animate(p, 
        renderer = av_renderer(tolower(paste('outputs/animation2/animation2_map', source, paste(area, collapse = "+"), paste(species, collapse = "+"), format, '.mp4', sep = '_'))), 
        width = ifelse(format == "landscape", 1920, 1080), 
        height = 1080, 
        res = 100, 
        duration = ifelse(format == "landscape", 30, 10), 
        fps = framerate, 
        start_pause = framerate, 
        end_pause = framerate, 
        type = "cairo")
utils::browseURL('map.mp4')