rm(list = ls())

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(gganimate)
library(showtext)
library(systemfonts)
library(magick)
library(future)

plan(multisession) # Activate multithreading
options(gganimate.dev = ragg::agg_png) # Make sure animate() uses AGG (which is multithreading safe) to create the frames

# Load functions

source("functions.R")

# Parameters

species <- c("Aquatic animals (Fish, crustaceans and molluscs, etc.)") # choices : "Aquatic animals (Fish, crustaceans and molluscs, etc.)", "Algae (Aquatic plants)"
source <- "Capture production"
source_title <- switch(source, "Aquaculture production" = "Aquaculture production", "Capture production" = "Capture fisheries production")
area <- c("Inland waters") # choices: "Inland waters" "Marine areas"
format <- "landscape"
annotations <- FALSE
framerate <- 30
video_length <- ifelse(format == "landscape", 30, 10)
frames_dir <- "outputs/frames/" # Output folder for frames without the logo
composited_dir <- "outputs/frames_with_logo/" # Output folder for frames with the logo
output_path <- tolower(paste('outputs/animation1/animation1', source, paste(area, collapse = "+"), species, format, '.mp4', sep = '_'))
color <- case_when(
  source == "Capture production" & all(area == "Marine areas") ~ "#7babc0",
  source == "Capture production" & all(area == "Inland waters") ~ "#0091a5",
  source == "Capture production" & length(area) > 1 ~ "#0091a5",
  source == "Aquaculture production" & all(area == "Marine areas") ~ "#b81f58",
  source == "Aquaculture production" & all(area == "Inland waters") ~ "#b81f58",
  source == "Aquaculture production" & length(area) > 1 ~ "#b81f58"
)
max_value_legend <- readRDS("inputs/data.RDS") %>%
  filter(species_group == species) %>%
  group_by(country, species_group, production_source_name, unit, year, lat, lon) %>%
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  head(1) %>%
  pull(value)
font_add_google(name = "Montserrat",
                family = "Montserrat",
                regular.wt = 400,
                bold.wt = 700) # Load FAO font

font_add_google(name = "Montserrat",
                family = "Montserrat-bold",
                regular.wt = 700,
                bold.wt = 700) # Load modified font to circumvent bug where some elements of the charts are not displayed as bold

showtext_auto() # Load custom fonts

image <- ifelse(format == "landscape",
                "./inputs/FAO_watermark/1920.png",
                "./inputs/FAO_watermark/1080.png")

# Import annotations

source("annotations.R")

# Import data

data_country_yearly <- readRDS("inputs/data.RDS") %>%
    filter(species_group == species, production_source_name == source, inland_marine_group_en %in% area) %>%
    group_by(country, continent_group_en, species_group, production_source_name, unit, year, lat, lon) %>%
    summarise(value = sum(value)) %>%
    filter(!is.na(lat), !is.na(lon), value > 0) %>% # ! Make sure all countries are assigned their coordinates !
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = "ESRI:54030") %>%
    # join annotations to the data
    left_join(annotations_country, by = c("country", "year")) %>%
    mutate(
      annotation = ifelse(year %in% unique(annotations_continent$year) & continent_group_en %in% unique(annotations_continent$continent_group_en), TRUE, annotation),
      annotation = ifelse(year %in% unique(annotations_country$year) & is.na(annotation), FALSE, annotation),
      color_code = ifelse(annotation | is.na(annotation), color, "gray"))
  
transition_times <- tibble(year = as.integer(sort(unique(data_country_yearly$year)))) %>%
  mutate(transition = ifelse(year %in% unique(annotations_country$year), 10, 1),
         state = ifelse(year %in% unique(annotations_country$year), 20, 0))
   
data_country_decadal <- readRDS("inputs/data.RDS") %>%
    filter(species_group == species, production_source_name == source, inland_marine_group_en %in% area) %>%
    group_by(country, species_group, production_source_name, unit, year, lat, lon) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(decade = as.integer(year - year %% 10)) %>%
    mutate(label = paste0(decade, "s")) %>%
    group_by(country, species_group, production_source_name, unit, decade, label, lat, lon) %>%
    summarise(value = mean(value)) %>%
    filter(!is.na(lat), !is.na(lon), value > 0) %>% # ! Make sure all countries are assigned their coordinates !
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = "ESRI:54030")

# Load shapefile

world <- read_sf(dsn = "./inputs/ne_110m_land/", layer = "ne_110m_land") %>%
  st_set_crs(4326) %>%
  st_transform(crs = "ESRI:54030")

world <- st_simplify(world, dTolerance = 1000, preserveTopology = TRUE) # Simplify world layer to render faster

# Plot map

if (format == "landscape") {
  
  p <- ggplot() +
    geom_sf(data = world, color = "lightgray", alpha = 0.7, size = 0.1) +
    coord_sf(datum = NA) +
    geom_sf(data = data_country_yearly, aes(geometry = geometry, group = country, size = value, color = ifelse(annotations, color_code, color)), alpha = 0.85) +
    {if(annotations) 
      geom_label(data = data_country_yearly, aes(x = lon, y = lat, label = comment), color = "black", alpha = 0.85)
      } +
    scale_size_area(
      max_size = ifelse(format == "landscape", 30, 20),
      breaks = scales::breaks_log(n = 6, base = 10)(1000:max_value_legend),
      limits = c(0, max_value_legend),
      labels = addUnits, 
      name = "Production"
    ) +
    scale_colour_identity(guide = "none") +
    guides(size = guide_legend(override.aes = list(colour = color))) + # to ensure the color of bubbles in the legend matches that of the bubbles on the map
    labs(title = paste0(source_title, " by country"),
         subtitle = paste0(ifelse(length(area) > 1, "", paste0(area, ", ")), paste(range(data_country_yearly$year), collapse = "-")), 
         tag = '{closest_state}',
         caption = 'Units: tonnes - live weight. The data only include aquatic animals.') +
    theme_void() +
    theme(text = element_text(colour = color, family = "Montserrat"),
          plot.title = element_text(hjust = 0.5, vjust = 0, margin = margin(t = 25), family = "Montserrat-bold", size = 35),
          plot.subtitle = element_text(hjust = 0.5, vjust = 0, size = 30),
          plot.caption = element_text(hjust = 0.5, size = 15),
          plot.tag = element_text(family = "Montserrat-bold", size = 80),
          plot.tag.location = "panel",
          plot.tag.position = c(0.8, 0.092),
          legend.position = "bottom",
          legend.margin = margin(b = 25),
          legend.direction = "horizontal",
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_text(size = 15, face = "bold")) +
    # gganimate part
    transition_states(year, transition_length = ifelse(annotations, transition_times$transition, 1), state_length = ifelse(annotations, transition_times$state, 0), wrap = FALSE) +
    ease_aes('linear') +
    enter_fade()
  
} else if (format == "square") {
  
  p <- ggplot() +
    geom_sf(data = world, color = "lightgray", alpha = 0.7, size = 0.1) +
    coord_sf(datum = NA) +
    geom_sf(data = data_country_decadal, aes(geometry = geometry, group = country, size = value), color = color, alpha = 0.85) +
    scale_size_area(
      max_size = ifelse(format == "landscape", 30, 20),
      breaks = scales::breaks_log(n = 6, base = 10)(1000:max_value_legend),
      limits = c(0, max_value_legend),
      labels = addUnits, 
      name = "Production"
    ) +
    labs(title = paste0(source_title, " by country"),
         subtitle = stringr::str_to_sentence(paste0(ifelse(length(area) > 1, "", paste0(area, ", ")), "decade average, ", paste(range(data_country_yearly$year), collapse = "-"))),
         tag = paste0('{closest_state}', 's'),
         caption = 'Units: tonnes - live weight. The data only include aquatic animals.') +
    theme_void() +
    theme(text = element_text(colour = color, family = "Montserrat"),
          plot.title = element_text(hjust = 0.5, vjust = 5, family = "Montserrat-bold", size = 25),
          plot.subtitle = element_text(hjust = 0.5, vjust = 5, size = 20),
          plot.caption = element_text(hjust = 0.5, size = 15),
          plot.tag = element_text(family = "Montserrat-bold", size = 65),
          plot.tag.location = "panel",
          plot.tag.position = c(0.8, 0.1),
          legend.position = "bottom",
          legend.margin = margin(b = 50),
          legend.direction = "horizontal",
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_text(size = 15, face = "bold")) +
    # gganimate part
    transition_states(decade, transition_length = 1, state_length = 0, wrap = FALSE) +
    ease_aes('linear') +
    enter_fade()
  
}

plot(p) # Generate a sample frame

# Generate the animation

source("rendering.R")