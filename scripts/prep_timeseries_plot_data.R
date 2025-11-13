library(qs)
library(tidyverse)
library(glue) 
library(here)

### read in all prediction data and save to rf_model_data folder

data_dir <- file.path("/homevol/fishingeffort/data_storage")

all_files <- list.files(here("rf_model_data_ind/"), full.names = TRUE)

all_data <- lapply(all_files, qs::qread) %>%
  bind_rows()

all_data_tidy <- all_data %>%
  group_by(year, flag_country_iso3c, flag_country_name, eez_sovereign_iso3c, 
           eez_sovereign_name, fao_fishing_id, fao_major_fishing_area, gear, length_category) %>%
  summarise(nom_active_fishing_days = sum(nom_active_fishing_days, na.rm = TRUE),
            eff_active_fishing_days = sum(eff_active_fishing_days, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sector = "Industrial")

  
  qs::qsave(all_data_tidy, here("timeseries_data/all_timeseries_data_grouped_ind.qs"))
  

  ### read in all prediction data and save to rf_model_data folder
  
  all_files <- list.files(here("rf_model_data_art/"), full.names = TRUE)
  
  all_data <- lapply(all_files, qs::qread) %>%
    bind_rows()
  
  all_data_tidy <- all_data %>%
    group_by(year, flag_country_iso3c, flag_country_name, eez_sovereign_iso3c, 
             eez_sovereign_name, fao_fishing_id, fao_major_fishing_area, length_category) %>%
    summarise(nom_active_fishing_days = sum(nom_active_fishing_days, na.rm = TRUE),
              eff_active_fishing_days = sum(eff_active_fishing_days, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(sector = "Artisanal")
  
  
  qs::qsave(all_data_tidy, here("timeseries_data/all_timeseries_data_grouped_art.qs"))
  

  