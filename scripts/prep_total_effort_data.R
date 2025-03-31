library(tidyverse)
library(qs)
library(countrycode)
library(janitor)

data_dir <- file.path("/homevol/fishingeffort/data_storage")

hist_fish_data <- qs::qread(here("data/rousseau_gear_fix.qs")) %>%
  ## remove artisanal
  filter(sector == "I") %>%
  group_by(year, flag_fin = country, gear = gear_new, length_category) %>%
  summarize(
    total_nominal_fishing_hours = sum(nom_active_hours, na.rm = TRUE),
    total_effective_fishing_hours = sum(eff_active_hours, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  dplyr::select(flag_fin, year, gear, length_category, total_nominal_fishing_hours, total_effective_fishing_hours) %>%
  filter(total_nominal_fishing_hours > 0) %>%
  mutate(flag_country_name = countrycode(flag_fin, origin = "iso3c", destination = "country.name"))

qs::qsave(hist_fish_data, here("data/total_effort_data.qs"))



# ## test to see how much of China's fishing effort we have per year
# 
chn_data <- qs::qread(here("rf_model_data/model_preds_1950_2017_TWN.qs")) %>%
  group_by(flag_fin, year, gear, length_category) %>%
  summarise(total_fishing = sum(nom_active_fishing_hours)) %>%
  ungroup()

chn_data <- qs::qread(here("rf_model_data/model_preds_1950_2017_TWN.qs")) %>%
  group_by(flag_fin, year, gear) %>%
  summarise(total_fishing = sum(nom_active_fishing_hours)) %>%
  ungroup()


join_df <- hist_fish_data %>%
  group_by(flag_fin, year, gear) %>%
  summarise(total_nominal_fishing_hours = sum(total_nominal_fishing_hours)) %>%
  ungroup( ) %>%
  filter(flag_fin == "TWN", year == 2017) %>%
  left_join(chn_data) %>%
  mutate(total_fishing = ifelse(is.na(total_fishing), 0, total_fishing)) %>%
  mutate(percent_covered = total_fishing/total_nominal_fishing_hours) 

# 
# test <- hist_fish_data %>%
#   filter(year == 2017, flag_fin == "ARG")
