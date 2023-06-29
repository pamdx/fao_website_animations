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

data_species <- readRDS("inputs/data.RDS") %>%
  filter(species_group %in% species, production_source_name == source, inland_marine_group_en %in% area) %>%
  group_by(production_source_name, isscaap_division_en, isscaap_group_en, unit, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  complete(year, nesting(production_source_name, isscaap_division_en, isscaap_group_en, unit), fill = list(value = 0)) %>% # This is needed to avoid weird easing effects when the number of groups per year isn't constant throughout the years
  group_by(year) %>%
  mutate(total = sum(value)) %>%
  mutate(percentage = value/total) %>%
  arrange(year, desc(isscaap_division_en)) %>%
  mutate(ypos = cumsum(percentage) - 0.5*percentage) %>% 
  mutate(yang = 90-(ypos*360)) %>% # Create an angle variable relative to the text position
  mutate(annotation_pie = ifelse(percentage >= 0.025, isscaap_group_en, "")) %>%
  mutate(annotation_chart = ifelse(percentage >= 0.01, isscaap_group_en, "")) %>%
  ungroup()

# data_species %>%
#   group_by(isscaap_division_en, year) %>%
#   summarise(n()) %>%
#   ggplot(aes(x = year, y = `n()`, color = isscaap_division_en)) +
#   geom_line() +
#   facet_wrap(vars(isscaap_division_en))

# Pie chart

p <- ggplot(data_species %>% filter(year %in% 1950:2021)) +
  geom_bar(mapping = aes(x = "", y = percentage, fill = isscaap_division_en, group = isscaap_division_en, alpha = isscaap_group_en), color = "lightgrey", width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(mapping = aes(x = "", y = percentage, angle = yang, label = annotation_pie), position = position_stack(vjust = 0.5), alpha = 0.5) +
  theme_void() +
  labs(fill = "ISSCAAP Division") +
  guides(alpha = "none") +
  labs(title = paste0(source, " by ISSCAAP division and group, ", ifelse(length(area) > 1, "", paste0(tolower(area), ", ")), paste(range(data_species$year), collapse = "-")), caption = ifelse(length(species) > 1, 'The data includes aquatic animals and plants. Units: tonnes - live weight (animals), wet weight (plants).', 'The data only includes aquatic animals. Units: tonnes - live weight.')) +
  # gganimate part
  labs(subtitle = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')

# Render animation

future::plan("multisession", workers = 8)
animate(p, renderer = av_renderer(tolower(paste('outputs/animation3/animation3_pie', source, paste(area, collapse = "+"), species, format, '.mp4', sep = '_'))), width = ifelse(format == "landscape", 1920, 1080), height = 1080, res = 100, duration = 20, fps = 30, start_pause = 30, end_pause = 30, type = "cairo")
utils::browseURL('pie.mp4')

# Bar chart

b <- ggplot(data_species %>% filter(year %in% 1950:2021)) +
  geom_bar(mapping = aes(x = reorder(isscaap_division_en, -percentage, sum), y = percentage, fill = isscaap_division_en, group = isscaap_division_en, alpha = isscaap_group_en), color = "lightgrey", width = 1, stat = "identity") +
  geom_text(mapping = aes(x = isscaap_division_en, y = percentage, label = annotation_chart), position = position_stack(vjust = 0.5), alpha = 0.5) +
  theme_classic() +
  guides(alpha = "none", fill = "none") +
  theme(axis.title.x = element_blank(), line = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = paste0(source, " by ISSCAAP division and group, ", ifelse(length(area) > 1, "", paste0(tolower(area), ", ")), paste(range(data_species$year), collapse = "-")), caption = ifelse(length(species) > 1, 'The data includes aquatic animals and plants. Units: tonnes - live weight (animals), wet weight (plants).', 'The data only includes aquatic animals. Units: tonnes - live weight.'), y = "Share of global production by quantity") +
  # gganimate part
  labs(subtitle = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')

# Render animation

future::plan("multisession", workers = 8)
animate(b, renderer = av_renderer(tolower(paste('outputs/animation3/animation3_bar', source, paste(area, collapse = "+"), species, format, '.mp4', sep = '_'))), width = ifelse(format == "landscape", 1920, 1080), height = 1080, res = 100, duration = 20, fps = 30, start_pause = 30, end_pause = 30, type = "cairo")
utils::browseURL('bar.mp4')