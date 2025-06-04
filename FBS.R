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

format <- "landscape"
framerate <- 30

# Import data

data_FBS_yearly <- readRDS("inputs/data_FBS.RDS") %>%
  filter(!is.na(lat), !is.na(lon), value != 0) %>% # ! Make sure all countries are assigned their coordinates !
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = "ESRI:54030")

data_FBS_decadal <- readRDS("inputs/data_FBS.RDS") %>%
  mutate(decade = as.integer(year - year %% 10)) %>%
  mutate(label = paste0(decade, "s")) %>%
  group_by(across(c(-year, -value))) %>%
  summarise(value = mean(value)) %>%
  filter(!is.na(lat), !is.na(lon), value != 0) %>% # ! Make sure all countries are assigned their coordinates !
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = "ESRI:54030")

# Load shapefile

world <- read_sf(dsn = "./inputs/ne_110m_land/", layer = "ne_110m_land") %>%
  st_set_crs(4326) %>%
  st_transform(crs = "ESRI:54030")

# Plot map

if (format == "landscape") {
  
  p <- ggplot() +
    geom_sf(data = world, color = "lightgray", alpha = 0.7, size = 0.1) +
    coord_sf(datum = NA) +
    geom_sf(data = data_FBS_yearly, aes(geometry = geometry, group = country_name, size = value), color ="#7babc0", alpha = 0.85) +
    scale_size_area(
      max_size = ifelse(format == "landscape", 30, 20),
      name = "Apparent consumption"
    ) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = "Apparent consumption of aquatic food by country",
         subtitle = paste(range(data_FBS_yearly$year), collapse = "-"), 
         caption = paste('{closest_state}', 'Units: KG/Cap/Year. The data only include aquatic animals.')) +
    # gganimate part
    transition_states(year, transition_length = 1, state_length = 0, wrap = FALSE) +
    ease_aes('linear') +
    enter_fade() # +
  # exit_fade()
  
  library(viridis)
  ggplot() +
    geom_sf(data = world, color = "lightgray", alpha = 0.7, size = 0.1) +
    coord_sf(datum = NA) +
    geom_sf(data = data_FBS_yearly %>% filter(year == max(year)), aes(geometry = geometry, group = country_name, size = value, color = value), alpha = 0.85) +
    scale_size_area(
      max_size = 10,
      name = "Apparent consumption"
    ) +
    scale_color_viridis() +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = "Apparent consumption of aquatic food by country",
         subtitle = "2019", 
         caption = 'Units: KG/Cap/Year. The data only include aquatic animals.') + 
    guides(color = "none")
  
  ggsave("adrienne.png", type = "cairo", bg = "white", width = 10.84, height = 6.1)
  
} else {
  
  p <- ggplot() +
    geom_sf(data = world, color = "lightgray", alpha = 0.7, size = 0.1) +
    coord_sf(datum = NA) +
    geom_sf(data = data_FBS_decadal, aes(geometry = geometry, group = country_name, size = value), color ="#7babc0", alpha = 0.85) +
    scale_size_area(
      max_size = ifelse(format == "landscape", 30, 20),
      name = "Apparent consumption"
    ) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = "Apparent consumption of aquatic food by country",
         subtitle = paste0('Decade average, ', paste(range(data_FBS_yearly$year), collapse = "-")),
         caption = paste(paste0('{closest_state}', 's'), 'KG/Cap/Year. The data only include aquatic animals.')) +
    # gganimate part
    transition_states(decade, transition_length = 1, state_length = 0, wrap = FALSE) +
    ease_aes('linear') +
    enter_fade() # +
  # exit_fade()
  
}

# Render animation

future::plan("multisession", workers = 8)
animate(p, renderer = av_renderer(tolower(paste('outputs/animation_FBS/animation_FBS', format, '.mp4', sep = '_'))), 
        width = ifelse(format == "landscape", 1920, 1080), 
        height = 1080, 
        res = 100, 
        duration = ifelse(format == "landscape", 30, 10), 
        fps = framerate, 
        start_pause = framerate, 
        end_pause = framerate, 
        type = "cairo")
utils::browseURL('countries_animation.mp4')
