library(qs)
library(tidyverse)
library(glue) 
library(here)
library(arrow)
library(data.table)

### read in all prediction data and save to rf_model_data folder

data_dir <- file.path("/homevol/fishingeffort/data_storage")

all_files <- list.files(file.path(data_dir, "prep/random_forest/zenodo_data/mapped_by_flag_country/"), full.names = TRUE)

for(file in all_files){
  
#  file <- all_files[4]
  
  data <- fread(file) %>%
    dplyr::select(-data_type) %>%
    mutate(eez_sovereign_name = case_when(
      eez_sovereign_name == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
      eez_sovereign_name == "Congo - Brazzaville" ~ "Republic of the Congo",
      TRUE ~ eez_sovereign_name
    ))
  
  
  if(nrow(data) == 0){
    next()
  }
  
  file_name <- str_replace(basename(file), "\\.csv", "\\.qs")
  
  qs::qsave(data, here(glue("rf_model_data_ind/{file_name}")))
  # write_parquet(data, here(glue("rf_model_data_ind/{file_name}"))) # parquet is the same size... 
  
}



data_dir <- file.path("/homevol/fishingeffort/data_storage")

all_files <- list.files(file.path(data_dir, "prep/random_forest/zenodo_data/artisanal_skylight_sentinel/mapped_by_flag_country/"), full.names = TRUE)

for(file in all_files){
  
  #  file <- all_files[118]
  
  data <- fread(file) %>%
    dplyr::select(-data_type, -pixel_area_m2, -nom_hours_km2, -nom_days_km2, -eff_hours_km2, -eff_days_km2, -pixel_id) %>%
    mutate(eez_sovereign_name = case_when(
      eez_sovereign_name == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
      eez_sovereign_name == "Congo - Brazzaville" ~ "Republic of the Congo",
      TRUE ~ eez_sovereign_name
    )) %>%
    mutate(flag_country_name = case_when(
      flag_country_name == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
      flag_country_name == "Congo - Brazzaville" ~ "Republic of the Congo",
      TRUE ~ flag_country_name
    ))
  
  
  if(nrow(data) == 0){
    next()
  }
  
  file_name <- str_replace(basename(file), "\\.csv", "\\.qs")
  
  qs::qsave(data, here(glue("rf_model_data_art/{file_name}")))
  
  
}

