library(qs)
library(tidyverse)
library(glue) 
library(here)

### read in all prediction data and save to rf_model_data folder

data_dir <- file.path("/homevol/fishingeffort/data_storage")

all_files <- list.files(file.path(data_dir, "prep/random_forest/zenodo_data/mapped_by_flag_country/"), full.names = TRUE)

for(file in all_files){
  
#  file <- all_files[118]
  
  data <- read.csv(file) %>%
    dplyr::select(-data_type, -pixel_area_m2, -nom_hours_km2, -nom_days_km2, -eff_hours_km2, -eff_days_km2, -pixel_id)
  
  if(nrow(data) == 0){
    next()
  }
  
  file_name <- str_replace(basename(file), "\\.csv", "\\.qs")
  
  qs::qsave(data, here(glue("rf_model_data_ind/{file_name}")))
  
  
}



data_dir <- file.path("/homevol/fishingeffort/data_storage")

all_files <- list.files(file.path(data_dir, "prep/random_forest/zenodo_data/artisanal_skylight_sentinel/mapped_by_flag_country/"), full.names = TRUE)

for(file in all_files){
  
  #  file <- all_files[118]
  
  data <- read.csv(file) %>%
    dplyr::select(-data_type, -pixel_area_m2, -nom_hours_km2, -nom_days_km2, -eff_hours_km2, -eff_days_km2, -pixel_id)
  
  if(nrow(data) == 0){
    next()
  }
  
  file_name <- str_replace(basename(file), "\\.csv", "\\.qs")
  
  qs::qsave(data, here(glue("rf_model_data_art/{file_name}")))
  
  
}

