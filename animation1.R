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

species <- "Fish, crustaceans and molluscs, etc."
source <- "Capture production"
area <- c("Inland waters")
format <- "landscape"

# Import data

data_country <- readRDS("inputs/data.RDS") %>%
  filter(species_group == species, production_source_name == source, inland_marine_group_en %in% area) %>%
  group_by(country, species_group, production_source_name, unit, year, lat, lon) %>%
  summarise(value = sum(value)) %>%
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
  geom_sf(data = data_country %>% filter(year %in% 1950:2021), aes(geometry = geometry, group = country, size = value, color = value), alpha = 0.85) +
  scale_size_continuous(
    range = c(1, ifelse(format == "landscape", 30, 20)) # adapt to aspect ratio
    , breaks = scales::breaks_log(n = 5, base = 10), labels = addUnits, name = "Production") +
  scale_color_continuous(breaks = scales::breaks_log(n = 5, base = 10), labels = addUnits, high = "#132B43", low = "#56B1F7", guide = "legend", name = "Production") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = paste0(source, " by country, ", ifelse(length(area) > 1, "", paste0(tolower(area), ", ")), paste(range(data_country$year), collapse = "-")), caption = 'Units: tonnes - live weight. The data only includes aquatic animals.') +
  # gganimate part
  labs(subtitle = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear') +
  enter_fade() +
  exit_shrink()

# Render animation

future::plan("multisession", workers = 8)
animate(p, renderer = av_renderer(tolower(paste('outputs/animation1/animation1', source, paste(area, collapse = "+"), species, format, '.mp4', sep = '_'))), width = ifelse(format == "landscape", 1920, 1080), height = 1080, res = 100, duration = 20, fps = 30, start_pause = 30, end_pause = 30, type = "cairo")
utils::browseURL('countries_animation.mp4')