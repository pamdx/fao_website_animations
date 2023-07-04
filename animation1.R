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

species <- "Fish, crustaceans and molluscs, etc."
source <- "Aquaculture production"
area <- c("Marine areas", "Inland waters")
format <- "square"
framerate <- 30

# Import data

data_country_yearly <- readRDS("inputs/data.RDS") %>%
  filter(species_group == species, production_source_name == source, inland_marine_group_en %in% area) %>%
  group_by(country, species_group, production_source_name, unit, year, lat, lon) %>%
  summarise(value = sum(value)) %>%
  filter(!is.na(lat), !is.na(lon), value != 0) %>% # ! Make sure all countries are assigned their coordinates !
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = "ESRI:54030")

data_country_decadal <- readRDS("inputs/data.RDS") %>%
  filter(species_group == species, production_source_name == source, inland_marine_group_en %in% area) %>%
  group_by(country, species_group, production_source_name, unit, year, lat, lon) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(decade = as.integer(year - year %% 10)) %>%
  group_by(country, species_group, production_source_name, unit, decade, lat, lon) %>%
  summarise(value = mean(value)) %>%
  filter(!is.na(lat), !is.na(lon), value != 0) %>% # ! Make sure all countries are assigned their coordinates !
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = "ESRI:54030")

# Load shapefile

world <- read_sf(dsn = "./inputs/ne_110m_land/", layer = "ne_110m_land") %>%
  st_set_crs(4326) %>%
  st_transform(crs = "ESRI:54030")

# Plot map

p <- ggplot() +
  geom_sf(data = world, color = "lightgray", alpha = 0.7, size = 0.1) +
  coord_sf(datum = NA) +
  geom_sf(data = data_country_yearly, aes(geometry = geometry, group = country, size = value), color = ifelse(length(area) > 1, "#659cb3", switch(area, "Marine areas" = "#0090a4", "Inland waters" = "#b1c91e")), alpha = 0.85) +
  scale_size_area(
    max_size = ifelse(format == "landscape", 30, 20),
    breaks = scales::breaks_log(n = 5, base = 10), labels = addUnits, name = "Production") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = paste0(source, " by country, ", ifelse(length(area) > 1, "", paste0(tolower(area), ", ")), paste(range(data_country_yearly$year), collapse = "-")), caption = 'Units: tonnes - live weight. The data only includes aquatic animals.') +
  # gganimate part
  labs(subtitle = 'Year: {frame_time}') +
  transition_time(year, range = 1950:2021) +
  ease_aes('linear') +
  enter_fade() +
  exit_shrink()

p <- ggplot() +
  geom_sf(data = world, color = "lightgray", alpha = 0.7, size = 0.1) +
  coord_sf(datum = NA) +
  geom_sf(data = data_country_decadal, aes(geometry = geometry, group = country, size = value), color = ifelse(length(area) > 1, "#659cb3", switch(area, "Marine areas" = "#0090a4", "Inland waters" = "#b1c91e")), alpha = 0.85) +
  scale_size_area(
    max_size = ifelse(format == "landscape", 30, 20),
    breaks = scales::breaks_log(n = 6, base = 10)(1000:max(data_country_decadal$value)), 
    labels = addUnits, 
    name = "Production"
    ) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = paste0(source, " by country, ", ifelse(length(area) > 1, "", paste0(tolower(area), ", ")), "decade average, ", paste(range(data_country_yearly$year), collapse = "-")), caption = 'Units: tonnes - live weight. The data only includes aquatic animals.') +
  # gganimate part
  labs(subtitle = paste0('Decade: {closest_state}', 's')) +
  transition_states(decade, transition_length = 1, state_length = 0, wrap = FALSE) +
  ease_aes('linear') +
  enter_fade() +
  exit_shrink()

# Render animation

future::plan("multisession", workers = 8)
animate(p, renderer = av_renderer(tolower(paste('outputs/animation1/animation1', source, paste(area, collapse = "+"), species, format, '.mp4', sep = '_'))), width = ifelse(format == "landscape", 1920, 1080), height = 1080, res = 100, duration = 10, fps = framerate, start_pause = framerate, end_pause = framerate, type = "cairo")
utils::browseURL('countries_animation.mp4')