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
source <- "Aquaculture production"
area <- c("Marine areas", "Inland waters") # choices: "Inland waters" "Marine areas"
format <- "landscape"
framerate <- 30
color <- ifelse(length(area) > 1, "#ce4a50", switch(area, "Marine areas" = "#7babc0", "Inland waters" = "#0091a5"))

# Build annotations dataframe

annotations_continent <- tribble(
  ~db, ~waters, ~continent_group_en, ~year, ~annotation, ~comment, ~lat, ~lon,
  "Capture production", "Inland waters", "Asia", 2005, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Africa", 2005, TRUE, NA, NA, NA,
  
  "Aquaculture production", "Inland waters", "Asia", 1987, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Americas", 1987, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Europe", 1987, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Africa", 1987, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Oceania", 1987, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Others", 1987, TRUE, NA, NA, NA,
  
  "Aquaculture production", "Inland waters", "Asia", 1999, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Americas", 1999, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Europe", 1999, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Africa", 1999, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Oceania", 1999, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Others", 1999, TRUE, NA, NA, NA,
  
  "Aquaculture production", "Inland waters", "Asia", 2021, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Americas", 2021, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Europe", 2021, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Africa", 2021, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Oceania", 2021, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Others", 2021, TRUE, NA, NA, NA,
  
) %>%
  filter(db == source, waters %in% area) %>%
  select(-c(db, waters))

annotations_country <- tribble(
  ~db, ~waters, ~country, ~year, ~annotation, ~comment, ~lat, ~lon,
  "Capture production", "Marine areas", "Peru", 1960, TRUE, "After considerably developing its marine fishing capabilities during the 1950s, \n Peru has consistently ranked among the world's top producers.", 687032.4, -6277719,
  "Capture production", "Marine areas", "South Africa", 1968, TRUE, "South Africa recorded its highest-ever marine capture production in 1968. \n With a catch of 2.1 million tonnes, it was the seventh largest producer in the world that year. \n Over two-thirds of that year's harvest consisted of Pacific sardine. Overfishing of the species meant that the \n size of the 1968 harvest has never been surpassed by the country.", -5512017, 2076621,
  "Capture production", "Marine areas", "Peru", 1983, TRUE, "The marine capture productions of Chile and Peru are highly dependent on the abundance of Anchoveta, \n which can make up more than 95% (Peru) or 80% (Chile) of these countries' total yearly marine catch. \n The species' abundance is subject to significant fluctuations, mostly due to the occurence of El Niño events. \n This explains the large fluctuations in these countries' productions.", 687032.4, -6277719,
  "Capture production", "Marine areas", "Chile", 1983, TRUE, NA, NA, NA,
  "Capture production", "Marine areas", "Japan", 1992, TRUE, "While Japan had remained in the top two largest marine capture producers until 1992, it has since then \n progressively lost importance relative to other major producing countries. \n It was the world's eighth largest marine capture producer in 2021.", 1503565, 10139899,
  "Capture production", "Marine areas", "China", 1995, TRUE, "China has been the world's largest marine capture producer since 1995.", 1697330, 9434398,
  "Capture production", "Marine areas", "United States of America", 2010, TRUE, "The United States of America and the USSR/Russian Federation have consistently \n ranked among the world's top 10 producers since FAO started collecting marine capture production data.", 2554652, 0,
  "Capture production", "Marine areas", "Russian Federation", 2010, TRUE, NA, NA, NA,
  "Capture production", "Marine areas", "Indonesia", 2020, TRUE, "South East Asia has become a cluster of major marine capture producers. \n Countries such as the Philippines, Thailand, Malaysia, Viet Nam and Indonesia \n have emerged through the years as global leaders in the sector.", -2703143, 7608000,
  "Capture production", "Marine areas", "Philippines", 2020, TRUE, NA, NA, NA,
  "Capture production", "Marine areas", "Viet Nam", 2020, TRUE, NA, NA, NA,
  "Capture production", "Marine areas", "Thailand", 2020, TRUE, NA, NA, NA,
  "Capture production", "Marine areas", "Malaysia", 2020, TRUE, NA, NA, NA,
  
  "Capture production", "Inland waters", "Bangladesh", 1955, TRUE, "Bangladesh has remained in the top five biggest inland producers ever since \n FAO started collecting statistics in 1950. However, the country has never been the world's largest producer.", 842038.2, 8328992,
  "Capture production", "Inland waters", "Uganda", 1965, TRUE, "Africa has remained a prominent source of inland capture production since 1950. \n A major cluster of producing countries are those with access to the African Great Lakes, \n which on average account for about 50% of the continent's yearly inland catches.", -5512017, 2076621,
  "Capture production", "Inland waters", "Tanzania, United Rep. of", 1965, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Congo, Dem. Rep. of the", 1965, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Kenya", 1965, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Malawi", 1965, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Zambia", 1965, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Mozambique", 1965, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Burundi", 1965, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Ethiopia", 1965, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Rwanda", 1965, TRUE, NA, NA, NA,
  "Capture production", "Inland waters", "Un. Sov. Soc. Rep.", 1985, TRUE, "The USSR was the biggest inland capture producer in the world every year from 1950 to 1987.", 4474936, 7830554,
  "Capture production", "Inland waters", "China", 1988, TRUE, "China subsequently held the top spot every year until 2019.", 1697330, 9434398,
  "Capture production", "Inland waters", "China", 2005, TRUE, "Since the 2000s, Asia has represented about 65% of global inland capture production, \n while Africa has maintained a relatively stable share of 25% of the global total.", -5512017, 2076621,
  "Capture production", "Inland waters", "India", 2020, TRUE, "India has been the world's largest inland capture producer since 2020.", 342543.2, 7319937,
  
  "Aquaculture production", "Inland waters", "France", 1950, TRUE, "The year 1950 is notable in the recent history of aquaculture production in that it was \n the only year when China was not world's largest producer. With 17% of world production, \n France was the largest aquaculture producing country in 1950. That year, over 98% of its \n production consisted of oysters and mussels.", 2868494, 186049.364330545,
  "Aquaculture production", "Inland waters", "China", 1981, TRUE, "For 15 years starting in 1981, China considerably increased its share of world aquaculture production. \n The country went from representing 28.1% of the global total to an all time high of 66.5% in 1996. \n The bulk of the country's production consists of carps, barbels and other cyprinids, oysters, clams, \n cockles, arkshells, miscellaneous freshwater fishes, and freshwater crustaceans", 1697330, 9434398,
  "Aquaculture production", "Inland waters", "China", 1987, TRUE, "1987: the share of aquaculture in global aquatic animal production exceeds 10% for the first time.", -5512017, 2076621,
  "Aquaculture production", "Inland waters", "China", 1999, TRUE, "1999: the share of aquaculture in global aquatic animal production exceeds 25% for the first time.", -5512017, 2076621,
  "Aquaculture production", "Inland waters", "Indonesia", 2003, TRUE, "Since the early 2000s, a group of three significant aquaculture producers (Indonesia, Viet Nam and Bangladesh) \n have been growing in importance on the global stage. Their combined share of global production has more than doubled \n in the last two decades: from 6% in 2000 to over 14% in 2021. Besides carps, barbels and other cyprinids, these countries also \n produce significant amounts of tilapias and other cichlids (Bangladesh) and shrimps and prawns (Indonesia, Viet Nam).", -2007158.19461269, 9434397.9901405,
  "Aquaculture production", "Inland waters", "Viet Nam", 2003, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "Bangladesh", 2003, TRUE, NA, NA, NA,
  "Aquaculture production", "Inland waters", "India", 2011, TRUE, "While India had represented a relatively stable share of around 6% to 7% of global aquaculture production since 1995, \n its share of world production has been rising fast in the past 10 years. In 2021, the country was the second \n largest aquaculture producer in the world, representing over 10% of global production by quantity. The country's \n aquaculture production mostly consists of carps, barbels and other cyprinids and miscellaneous freshwater fishes", 342543.2, 7319937,
  "Aquaculture production", "Inland waters", "China", 2021, TRUE, "In 2021, global aquaculture production represented just under 50% of the world's production of aquatic animals. \n This share is expected to grow to 53% by 2030.", -5512017, 2076621,
  
  'Aquaculture production', 'Inland waters', 'Afghanistan', 2017, TRUE, "Aquaculture production was larger than capture production at least one year since 1950 for the 50 countries highlighted \n on the map, which shows the growing importance of aquaculture for food security in many regions of the world.", -5512017, 2076621,
  'Aquaculture production', 'Inland waters', 'Albania', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Armenia', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Austria', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Bangladesh', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Belarus', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Bhutan', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Bosnia and Herzegovina', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Botswana', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Bulgaria', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'China', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Colombia', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Costa Rica', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Cuba', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Cyprus', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Czechia', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Czechoslovakia', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Ecuador', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Egypt', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Eswatini', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Greece', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Guam', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Guatemala', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Honduras', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Hungary', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'India', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Iraq', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Israel', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Jordan', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Kyrgyzstan', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', "Lao People's Dem. Rep.", 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Lesotho', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Malta', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Moldova, Republic of', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Montenegro', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Nepal', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'North Macedonia', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Romania', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Saudi Arabia', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Serbia', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Serbia and Montenegro', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Singapore', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Slovakia', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Slovenia', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Switzerland', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Syrian Arab Republic', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Tajikistan', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Türkiye', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Uzbekistan', 2017, TRUE, NA, NA, NA,
  'Aquaculture production', 'Inland waters', 'Viet Nam', 2017, TRUE, NA, NA, NA
  
) %>%
  filter(db == source, waters %in% area) %>%
  select(-c(db, waters))

# Import data

data_country_yearly <- readRDS("inputs/data.RDS") %>%
    filter(species_group == species, production_source_name == source, inland_marine_group_en %in% area) %>%
    group_by(country, continent_group_en, species_group, production_source_name, unit, year, lat, lon) %>%
    summarise(value = sum(value)) %>%
    filter(!is.na(lat), !is.na(lon), value != 0) %>% # ! Make sure all countries are assigned their coordinates !
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
    geom_sf(data = data_country_yearly, aes(geometry = geometry, group = country, size = value, color = color_code), alpha = 0.85) +
    geom_label(data = data_country_yearly, aes(x = lon, y = lat, label = comment), color = "black", alpha = 0.85) +
    scale_size_area(
      max_size = ifelse(format == "landscape", 30, 20),
      breaks = scales::breaks_log(n = 6, base = 10)(1000:max(data_country_yearly$value)), 
      labels = addUnits, 
      name = "Production"
    ) +
    scale_colour_identity() +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = paste0(source, " by country"),
         subtitle = paste0(ifelse(length(area) > 1, "", paste0(area, ", ")), paste(range(data_country_yearly$year), collapse = "-")), 
         caption = 'Units: tonnes - live weight. The data only include aquatic animals.') +
    geom_text(data = data_country_yearly, aes(label = as.factor(year)), x = 15000000, y = -7700000, alpha = 0.2,  col = "gray", size = 20) +
    # gganimate part
    # labs(subtitle = 'Year: {closest_state}') +
    transition_states(year, transition_length = transition_times$transition, state_length = transition_times$state, wrap = FALSE) +
    ease_aes('linear') +
    enter_fade() # +
    # exit_fade()
  
} else {
  
  p <- ggplot() +
    geom_sf(data = world, color = "lightgray", alpha = 0.7, size = 0.1) +
    coord_sf(datum = NA) +
    geom_sf(data = data_country_decadal, aes(geometry = geometry, group = country, size = value), color = color, alpha = 0.85) +
    scale_size_area(
      max_size = ifelse(format == "landscape", 30, 20),
      breaks = scales::breaks_log(n = 6, base = 10)(1000:max(data_country_decadal$value)), 
      labels = addUnits, 
      name = "Production"
    ) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = paste0(source, " by country"),
         subtitle = stringr::str_to_sentence(paste0(ifelse(length(area) > 1, "", paste0(area, ", ")), "decade average, ", paste(range(data_country_yearly$year), collapse = "-"))), 
         caption = 'Units: tonnes - live weight. The data only include aquatic animals.') +
    geom_text(data = data_country_decadal, aes(label = as.factor(label)), x = 15000000, y = -7700000, alpha = 0.2,  col = "gray", size = 20) +
    # gganimate part
    # labs(subtitle = 'Year: {closest_state}') +
    transition_states(decade, transition_length = 1, state_length = 0, wrap = FALSE) +
    ease_aes('linear') +
    enter_fade() # +
    # exit_fade()
  
}

# Render animation

future::plan("multisession", workers = 8)
animate(p, renderer = av_renderer(tolower(paste('outputs/animation1/animation1', source, paste(area, collapse = "+"), species, format, '.mp4', sep = '_'))), 
        width = ifelse(format == "landscape", 1920, 1080), 
        height = 1080, 
        res = 100, 
        duration = ifelse(format == "landscape", 60, 10), 
        fps = framerate, 
        start_pause = framerate, 
        end_pause = framerate, 
        type = "cairo")
utils::browseURL('countries_animation.mp4')