rm(list = ls())

library(dplyr)
library(tidyr)
library(sf)
library(viridis)
library(gganimate)
library(showtext)

font_add_google("Montserrat") # Load FAO font
showtext_auto()

source("functions.R")

# Parameters

species <- c("Fish, crustaceans and molluscs, etc.")
source <- "Capture production"
area <- c("Inland waters")
format <- "landscape"

# Import data

data_fishing_area <- readRDS("inputs/data.RDS") %>%
  filter(species_group %in% species, production_source_name == source, inland_marine_group_en %in% area) %>%
  group_by(production_source_name, fishing_area_code, fishing_area_name, unit, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  complete(year, nesting(production_source_name, fishing_area_code, fishing_area_name, unit), fill = list(value = 0)) %>% # This is needed to avoid weird easing effects when the number of groups per year isn't constant throughout the years
  separate(fishing_area_name, c("fishing_area_group", "fishing_area_subgroup"), ", ", remove = FALSE) %>% # Much clearer result when grouping by ocean
  group_by(year) %>%
  mutate(total = sum(value)) %>%
  mutate(percentage = value/total) %>%
  arrange(year, desc(fishing_area_name)) %>%
  mutate(ypos = cumsum(percentage) - 0.5*percentage) %>% 
  mutate(yang = 90-(ypos*360)) %>% # Create an angle variable relative to the text position
  mutate(annotation_pie = ifelse(percentage >= 0.025, fishing_area_name, "")) %>%
  mutate(annotation_bar = ifelse(percentage >= 0.01, fishing_area_subgroup, "")) %>%
  mutate(annotation_map = ifelse(percentage >= 0.01, scales::label_percent(accuracy = 1)(percentage), ""))

# Pie chart

p <- ggplot(data_fishing_area %>% filter(year %in% 1950:2021, fishing_area_group != "Arctic Sea")) +
  geom_bar(mapping = aes(x = "", y = percentage, fill = fishing_area_group, group = fishing_area_group, alpha = fishing_area_subgroup), width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(mapping = aes(x = "", y = percentage, angle = yang, label = annotation_pie), position = position_stack(vjust = 0.5), alpha = 0.5) +
  theme_void() +
  labs(fill = "Fishing area") +
  guides(alpha = "none") +
  labs(title = paste0(source, " by fishing area, ", ifelse(length(area) > 1, "", paste0(tolower(area), ", ")), paste(range(data_fishing_area$year), collapse = "-")), caption = ifelse(length(species) > 1, 'The data includes aquatic animals and plants. Units: tonnes - live weight (animals), wet weight (plants).', 'The data only includes aquatic animals. Units: tonnes - live weight.')) +
  # gganimate part
  labs(subtitle = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear') +
  enter_fade() +
  exit_shrink()

# Render animation

future::plan("multisession", workers = 8)
animate(p, renderer = av_renderer(tolower(paste('outputs/animation2/animation2_pie', source, paste(area, collapse = "+"), paste(species, collapse = "+"), format, '.mp4', sep = '_'))), width = ifelse(format == "landscape", 1920, 1080), height = 1080, res = 100, duration = 20, fps = 30, start_pause = 30, end_pause = 30, type = "cairo")
utils::browseURL('pie.mp4')

# Bar chart

b <- ggplot(data_fishing_area %>% filter(year %in% 1950:2021, fishing_area_group != "Arctic Sea")) +
  geom_bar(mapping = aes(x = reorder(fishing_area_group, -percentage, sum), y = percentage, fill = fishing_area_group, group = fishing_area_subgroup, alpha = fishing_area_subgroup), width = 1, stat = "identity") +
  geom_text(mapping = aes(x = fishing_area_group, y = percentage, label = annotation_bar), position = position_stack(vjust = 0.5), alpha = 0.5) +
  theme_classic() +
  guides(alpha = "none", fill = "none") +
  theme(axis.title.x = element_blank(), line = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0(source, " by fishing area, ", ifelse(length(area) > 1, "", paste0(tolower(area), ", ")), paste(range(data_fishing_area$year), collapse = "-")), caption = ifelse(length(species) > 1, 'The data includes aquatic animals and plants. Units: tonnes - live weight (animals), wet weight (plants).', 'The data only includes aquatic animals. Units: tonnes - live weight.'), y = "Share of global production by quantity") +
  # gganimate part
  labs(subtitle = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear') +
  enter_fade() +
  exit_shrink()

# Render animation

future::plan("multisession", workers = 8)
animate(b, renderer = av_renderer(tolower(paste('outputs/animation2/animation2_bar', source, paste(area, collapse = "+"), paste(species, collapse = "+"), format, '.mp4', sep = '_'))), width = ifelse(format == "landscape", 1920, 1080), height = 1080, res = 100, duration = 20, fps = 30, start_pause = 30, end_pause = 30, type = "cairo")
utils::browseURL('bar.mp4')

# Plot map  

centroids <- read_sf('./inputs/FAO_fishing_areas_shp/FAO_fishing_areas_shp.shp') %>% select(F_CODE, Lat, Lon) %>%
  add_row(F_CODE = 7, Lat = 7830554, Lon = 6484471) %>% # add missing coordinate for USSR inland areas
  mutate(Lat = ifelse(F_CODE == 5, 365210.2, Lat), Lon = ifelse(F_CODE == 5, 5378969, Lon)) %>% # relocate Europe's bubble to western Europe to avoid confusion with the USSR bubble
  st_drop_geometry()

data_fishing_area_map <- data_fishing_area %>%
  left_join(centroids, by = c("fishing_area_code" = "F_CODE")) %>%
  st_as_sf(coords = c("Lat", "Lon"), crs = "ESRI:54030", remove = FALSE)

fishing_areas_shp <- read_sf('./inputs/FAO_fishing_areas_shp/FAO_fishing_areas_shp.shp') %>%
  st_transform("ESRI:54030") %>% # Reproject as Robinson
  st_simplify(preserveTopology = FALSE, dTolerance = 20000) %>% # Simplify polygons
  select(-c(Lat, Lon)) %>%
  mutate(water_type = ifelse(water_type == "Marine waters", "Marine areas", water_type)) %>%
  filter(water_type %in% area)

p <- ggplot() +
  geom_sf(data = fishing_areas_shp, fill = "white", color = "darkgray", alpha = 0.7, size = 0.15) +
  geom_sf(data = data_fishing_area_map %>% filter(year %in% 1950:2021, value != 0), aes(geometry = geometry, group = fishing_area_code, size = value , color = value), alpha = 0.85) +
  geom_text(data = data_fishing_area_map, aes(x = Lat, y = Lon, label = annotation_map), alpha = 0.85) +
  scale_size_continuous(range = c(1, ifelse(format == "landscape", 60, 40))
                        , breaks = scales::breaks_log(n = 5, base = 10), labels = addUnits, name = "Production") +
  scale_color_continuous(breaks = scales::breaks_log(n = 5, base = 10), labels = addUnits, high = "#132B43", low = "#56B1F7", guide = "legend", name = "Production") +
  theme_void() + 
  guides(size = "none") +
  theme(plot.background = element_rect(fill = "white"), legend.position = "bottom") +
  labs(title = paste0(source, " by fishing area, ", ifelse(length(area) > 1, "", paste0(tolower(area), ", ")), paste(range(data_fishing_area$year), collapse = "-")), caption = ifelse(length(species) > 1, 'The data includes aquatic animals and plants. Units: tonnes - live weight (animals), wet weight (plants).', 'The data only includes aquatic animals. Units: tonnes - live weight.')) +
  # gganimate part
  labs(subtitle = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear') +
  enter_fade() +
  exit_shrink()

# Render animation

future::plan("multisession", workers = 8)
animate(p, renderer = av_renderer(tolower(paste('outputs/animation2/animation2_map', source, paste(area, collapse = "+"), paste(species, collapse = "+"), format, '.mp4', sep = '_'))), width = ifelse(format == "landscape", 1920, 1080), height = 1080, res = 100, duration = 20, fps = 30, start_pause = 30, end_pause = 30, type = "cairo")
utils::browseURL('map.mp4')