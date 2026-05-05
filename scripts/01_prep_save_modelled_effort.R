library(qs)
library(tidyverse)
library(glue) 
library(here)
library(arrow)
library(data.table)

### read in all prediction data and save to rf_model_data folder

data_dir <- file.path("/homevol/fishingeffort/data_storage")

all_files <- list.files(file.path(data_dir, "prep/random_forest/zenodo_data/mapped_by_flag_country_industrial/"), full.names = TRUE)

# all_files <- list.files(file.path(data_dir, "prep/random_forest/predictions_flag_no_access/"), full.names = TRUE)[-148]

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

all_files <- list.files(file.path(data_dir, "prep/random_forest/zenodo_data/artisanal_skylight_sentinel/mapped_by_flag_country_artisanal/"), full.names = TRUE)

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

## lets save a combined (Industrial + Artisanal map)
all_files_art <- list.files(here("rf_model_data_art/"), full.names = TRUE)

art_df <- lapply(all_files_art, qread) %>%
  bind_rows() 

art_1 <- art_df %>%
  mutate(
    lon_1deg = floor(lon + 0.5),
    lat_1deg = floor(lat + 0.5)
  ) %>%
  group_by(
    lon = lon_1deg,
    lat = lat_1deg,
    year,
    flag_country_iso3c,
    length_category,
    sector,
    flag_country_name,
    eez_sovereign_iso3c,
    eez_sovereign_name,
    fao_fishing_id,
    fao_major_fishing_area
  ) %>%
  summarise(
    nv = sum(nv, na.rm = TRUE),
    nom_active_fishing_days = sum(nom_active_fishing_days, na.rm = TRUE),
    eff_active_fishing_days = sum(eff_active_fishing_days, na.rm = TRUE),
    nom_active_fishing_hours = sum(nom_active_fishing_hours, na.rm = TRUE),
    eff_active_fishing_hours = sum(eff_active_fishing_hours, na.rm = TRUE),
    pixel_area_km2 = sum(pixel_area_km2),
    .groups = "drop"
  )



all_files_ind <- list.files(here("rf_model_data_ind/"), full.names = TRUE)

ind_df <- lapply(all_files_ind, qread) %>%
  bind_rows()

ind_df_prep <- ind_df %>%
  group_by(
    lon,
    lat,
    year,
    flag_country_iso3c,
    length_category,
    sector,
    flag_country_name,
    eez_sovereign_iso3c,
    eez_sovereign_name,
    fao_fishing_id,
    fao_major_fishing_area
  ) %>%
  summarise(
    nv = sum(nv, na.rm = TRUE),
    nom_active_fishing_days = sum(nom_active_fishing_days, na.rm = TRUE),
    eff_active_fishing_days = sum(eff_active_fishing_days, na.rm = TRUE),
    nom_active_fishing_hours = sum(nom_active_fishing_hours, na.rm = TRUE),
    eff_active_fishing_hours = sum(eff_active_fishing_hours, na.rm = TRUE),
    .groups = "drop"
  )


pixel_areas_df_ind <- ind_df %>%
  distinct(lon, lat, pixel_area_km2_ind = pixel_area_km2) %>%
  mutate(lon_lat = paste(lon, lat, sep = "_"))


pixel_areas_df_art <- art_1 %>%
  distinct(lon, lat, pixel_area_km2_art = pixel_area_km2) %>%
  mutate(lon_lat = paste(lon, lat, sep = "_")) %>%
  filter(!(lon_lat %in% unique(pixel_areas_df_ind$lon_lat))) %>%
  dplyr::select(-lon_lat)

pixel_areas_df_art <- art_df %>%
  mutate(
    lon = floor(lon + 0.5),
    lat = floor(lat + 0.5)
  ) %>%
  group_by(lon, lat) %>%
  summarise(
    pixel_area_km2_art = sum(unique(pixel_area_km2), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  anti_join(pixel_areas_df_ind, by = c("lon", "lat"))


all_df <- ind_df_prep %>%
  rbind(art_1 %>%
          dplyr::select(-pixel_area_km2)) %>%
  group_by(
    lon,
    lat,
    year,
    flag_country_iso3c,
    length_category,
    flag_country_name,
    eez_sovereign_iso3c,
    eez_sovereign_name,
    fao_fishing_id,
    fao_major_fishing_area
  ) %>%
  summarise(
    nv = sum(nv, na.rm = TRUE),
    nom_active_fishing_days = sum(nom_active_fishing_days, na.rm = TRUE),
    eff_active_fishing_days = sum(eff_active_fishing_days, na.rm = TRUE),
    nom_active_fishing_hours = sum(nom_active_fishing_hours, na.rm = TRUE),
    eff_active_fishing_hours = sum(eff_active_fishing_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(sector = "Combined (Industrial + Artsianal)")

all_df_pixel_area <- all_df %>%
  left_join(pixel_areas_df_ind) %>%
  left_join(pixel_areas_df_art) %>%
  mutate(pixel_area_km2 = 
           case_when(
             is.na(pixel_area_km2_art) & !is.na(pixel_area_km2_ind) ~ pixel_area_km2_ind,
             !is.na(pixel_area_km2_art) & is.na(pixel_area_km2_ind) ~ pixel_area_km2_art,
             TRUE ~ NA
           )) %>%
  dplyr::select(-pixel_area_km2_art, -pixel_area_km2_ind, -lon_lat)

test <- all_df_pixel_area %>%
  filter(is.na(pixel_area_km2)) # 0

flags <- unique(all_df_pixel_area$flag_country_iso3c)

for(flag_nm in flags){
  
  # flag_nm = "USA"
  
  data <- all_df_pixel_area %>%
    filter(flag_country_iso3c == flag_nm)
  
  
  if(nrow(data) == 0){
    next()
  }
  
  
  qs::qsave(data, here(glue("rf_model_data_combined/model_preds_1950_2017_{flag_nm}.qs")))
  
  
}

