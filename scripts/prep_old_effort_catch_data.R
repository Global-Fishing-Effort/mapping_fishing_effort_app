library(tidyverse)
library(qs)
library(countrycode)
library(janitor)

data_dir <- file.path("/homevol/fishingeffort/data_storage")
## read in the per flag country, FAO, EEZ (fishing location), gear, length, fgroup, fishing effort and catch data that Cami created at https://github.com/Fish-MIP/FishingEffort/tree/main

fishing_catch_data <- read.csv(file.path(data_dir, "raw_data/fishing_effort_catch_from_gem/catch_histsoc_1869_2017_EEZ_addFAO.csv"))


prep_catch <- fishing_catch_data %>%
  clean_names() %>%
  rename(flag_fin = iso3) %>%
  mutate(total_catch = reported + iuu + discards) %>%
  dplyr::group_by(year, saup, fao_area, eez_country_name, sector, f_country_name, flag_fin, f_group) %>%
  summarise(total_catch = sum(total_catch, na.rm = TRUE)) %>% 
  ungroup()

fishing_effort_data <- read.csv(file.path(data_dir, "raw_data/fishing_effort_catch_from_gem/effort_histsoc_1869_2017_EEZ_addFAO.csv"))