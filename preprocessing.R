##### UPDATE DATA INPUTS #####

library(readr)
library(dplyr)
library(janitor)

# Get production data from FAO's server

temp <- tempfile()
download.file("https://www.fao.org/fishery/static/Data/GlobalProduction_2023.1.1.zip", temp)
data <- read_csv(unz(temp, "Global_production_quantity.csv")) %>%
  clean_names()
countries <- read_csv(unz(temp, "CL_FI_COUNTRY_GROUPS.csv"), na = "") %>% # adding na = "" so that Namibia's ISO2 code isn't interpreted as a missing value
  clean_names() %>%
  select(un_code, iso2_code, name_en, continent_group_en, geo_region_group_en)
species <- read_csv(unz(temp, "CL_FI_SPECIES_GROUPS.csv")) %>%
  clean_names() %>%
  select(x3a_code, name_en, scientific_name, isscaap_group_en, yearbook_group_en)
water <- read_csv(unz(temp, "CL_FI_WATERAREA_GROUPS.csv")) %>%
  clean_names() %>%
  select(code, name_en, inland_marine_group_en)
source <- read_csv(unz(temp, "CL_FI_PRODUCTION_SOURCE_DET.csv")) %>%
  clean_names() %>%
  select(code, name_en)
unlink(temp)

# Join tables, restructure and export data

prod_raw <- data %>%
  left_join(countries, by = c("country_un_code" = "un_code"), keep = FALSE) %>%
  rename(un_code = country_un_code, country = name_en) %>%
  left_join(species, by = c("species_alpha_3_code" = "x3a_code"), keep = FALSE) %>%
  rename(`3alpha_code` = species_alpha_3_code, species_name = name_en) %>%
  left_join(water, by = c("area_code" = "code"), keep = FALSE) %>%
  rename(fishing_area_code = area_code, fishing_area_name = name_en) %>%
  left_join(source, by = c("production_source_det_code" = "code"), keep = FALSE) %>%
  rename(production_source_detailed_name = name_en) %>%
  mutate(production_source_name = case_when(
    production_source_detailed_name == "Aquaculture production (freshwater)" ~ "Aquaculture production", 
    production_source_detailed_name == "Aquaculture production (brackishwater)" ~ "Aquaculture production", 
    production_source_detailed_name == "Aquaculture production (marine)" ~ "Aquaculture production",
    production_source_detailed_name == "Capture production" ~ "Capture production",
    production_source_detailed_name == "Aquaculture production" ~ "Aquaculture production")
  ) %>%
  mutate(measure = case_when(
    measure == "Q_tlw" ~ "Tonnes - live weight", 
    measure == "Q_no_1" ~ "Number")
  ) %>%
  rename(unit = measure,	year = period,	flag = status) %>%
  select(un_code, iso2_code, country, continent_group_en, geo_region_group_en, `3alpha_code`, species_name, scientific_name, isscaap_group_en, yearbook_group_en, fishing_area_code, fishing_area_name, inland_marine_group_en, production_source_detailed_name, production_source_name, unit, year, value, flag) %>%
  group_by_at(vars(-value)) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(year = as.integer(year)) %>%
  mutate(fishing_area_code = as.integer(fishing_area_code))

# Join geographical coordinates

prod_raw$iso2_code[prod_raw$country == "Czechoslovakia"] <- "CZ" # Fix lack of ISO2 for Czechoslovakia
prod_raw$iso2_code[prod_raw$country == "Sudan (former)"] <- "SD" # Fix lack of ISO2 for Sudan (former)
prod_raw$iso2_code[prod_raw$country == "United Republic of Tanzania, Zanzibar"] <- "ZZ" # Fix lack of ISO2 for Zanzibar (unofficial)
prod_raw$iso2_code[prod_raw$country == "Channel Islands"] <- "CP" # Fix lack of ISO2 for Channel Islands (unofficial)
prod_raw$iso2_code[prod_raw$country == "Sint Maarten"] <- "SX" # Fix lack of ISO2 for Sint Maarten (unofficial)
prod_raw$iso2_code[prod_raw$country == "Saint-Martin (French)"] <- "SF" # Fix lack of ISO2 for Saint-Martin (French) (unofficial)
prod_raw$iso2_code[prod_raw$country == "Saint Barthélemy"] <- "SF" # Fix lack of ISO2 for Saint Barthélemy (unofficial)

cou_coordinates <- read_csv("https://raw.githubusercontent.com/google/dspl/master/samples/google/canonical/countries.csv", na = "") %>%
  rename(ISO2 = country) %>%
  select(ISO2, latitude, longitude) %>%
  add_row(ISO2 = "XX", latitude = -50, longitude = 0) %>% # Add Other nei's coordinates
  add_row(ISO2 = "SU", latitude = 61.524010, longitude = 105.318756) %>% # Add USSR using Russia's coordinates
  add_row(ISO2 = "BQ", latitude = 12.201890, longitude = -68.262383) %>% # Add Bonaire's coordinates
  add_row(ISO2 = "CW", latitude = 12.169570, longitude = -68.990021) %>% # Add Curaçao's coordinates
  add_row(ISO2 = "CS", latitude = 44.016521, longitude = 21.005859) %>% # Add Serbia and Montenegro using Serbia's coordinates
  add_row(ISO2 = "SS", latitude = 4.859363, longitude = 31.571251) %>% # Add South Sudan's coordinates
  add_row(ISO2 = "YU", latitude = 44.016521, longitude = 21.005859) %>% # Add Yugoslavia using Serbia's coordinates
  add_row(ISO2 = "ZZ", latitude = -6.165917, longitude = 39.202641) %>% # Add Zanzibar using its capital's coordinates
  add_row(ISO2 = "CP", latitude = 49.354417, longitude = -2.372106) %>% # Add Channel Islands using an arbitrary point in the English Channel
  add_row(ISO2 = "SX", latitude = 18.0237, longitude = -63.0458) %>% # Add Sint Maarten using its capital's coordinates
  add_row(ISO2 = "SF", latitude = 18.0731, longitude = -63.0822) %>% # Add Saint-Martin (French) using its capital's coordinates
  add_row(ISO2 = "SW", latitude = 17.897908, longitude = -62.850556) # Add Saint Barthélemy using its capital's coordinates

prod_raw <- prod_raw %>%
  left_join(y = cou_coordinates, by = c("iso2_code" = "ISO2")) %>%
  rename(lat = latitude, lon = longitude)

# Aggregate data at species group level

isscaap_classif <- read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ISSCAAP_GROUP.csv", col_types = "ccccc", col_select = c(ISSCAAP_Code, Name_En)) %>%
  rename(isscaap_group_code = ISSCAAP_Code, isscaap_group_en = Name_En) %>%
  mutate(conc_isscaap_group = paste(isscaap_group_code, "-", isscaap_group_en),
         isscaap_division_code = substr(isscaap_group_code, 1, 1)) %>%
  left_join(read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ISSCAAP_DIVISION.csv", col_types = "ccccc", col_select = c(ISSCAAP_Code, Name_En)), by = c("isscaap_division_code" = "ISSCAAP_Code")) %>%
  rename(isscaap_division_en = Name_En) %>%
  mutate(conc_isscaap_division = paste(isscaap_division_code, "-", isscaap_division_en))

# Final data export

prod_final <- prod_raw %>%
  left_join(isscaap_classif) %>%
  group_by(country, continent_group_en, yearbook_group_en, production_source_name, isscaap_division_en, isscaap_group_en, inland_marine_group_en, fishing_area_code, fishing_area_name, unit, year, lat, lon) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(species_group = yearbook_group_en)

saveRDS(prod_final, "inputs/data.RDS")
