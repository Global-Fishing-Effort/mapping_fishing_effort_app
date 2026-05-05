# Here we need to prep the data from Rousseau et al. I think we can split it into 
# multiple files (industrial and artisanal), and save as parquets? Then when reading
# into the app, we can read in as parquet and filter that way, which should speed things up.

## first we will save the already aggregated data for the time series tab
library(tidyverse)
library(qs)
library(arrow)
library(here)
library(janitor)
library(strex)
library(glue)

data_dir <- file.path("/homevol/fishingeffort/data_storage")

countries <- read.csv("https://data.imas.utas.edu.au/attachments/1241a51d-c8c2-4432-aa68-3d2bae142794/SAUPtoCountry.csv") %>%
  clean_names()

## Effort
eff_df <- open_dataset(file.path(data_dir, "raw_data/fishing_effort_catch_from_gem/all_effort_aggregated_EEZ.parquet"))

test <- qs::qread(here("timeseries_data/all_timeseries_data_grouped_ind.qs")) %>%
  rbind(qs::qread(here("timeseries_data/all_timeseries_data_grouped_art.qs")) %>%
          mutate(gear = NA))
colnames(test)
# [1] "year"                    "flag_country_iso3c"      "flag_country_name"       "eez_sovereign_iso3c"    
# [5] "eez_sovereign_name"      "fao_fishing_id"          "fao_major_fishing_area"  "gear"                   
# [9] "length_category"         "nom_active_fishing_days" "eff_active_fishing_days" "sector" 


fao_lookup <- test %>%
  distinct(fao_fishing_id, fao_major_fishing_area)

eez_lookup <- test %>%
  distinct(eez_sovereign_iso3c, eez_sovereign_name)

flag_lookup <- test %>%
  distinct(flag_country_iso3c, flag_country_name)
  
## need to add fao_major_fishing_area (name), flag_country_name (countrycode), eez_sovereign_iso3c (countrycode)

eff_df_clean <- eff_df %>%
  filter(year >=1950) %>%
  left_join(countries) %>%
  dplyr::select(year, flag_country_iso3c = country, eez_sovereign_name = eez_name, 
                fao_fishing_id = fao_region_id, gear, f_group, sector, nom_active, eff_active, length_category) %>%
  group_by(year, eez_sovereign_name, flag_country_iso3c, fao_fishing_id, gear, length_category, f_group, sector) %>%
  summarise(nom_active = sum(nom_active, na.rm = TRUE),
            eff_active = sum(eff_active, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(nom_active > 0) %>%
  left_join(fao_lookup) %>%
  left_join(eez_lookup) %>%
  left_join(flag_lookup)

# test <- eff_df_clean %>%
#   collect()
# 
# # need to fix any missing
# library(countrycode)
# fix_eez <- test %>%
#   distinct(eez_sovereign_name, eez_sovereign_iso3c) %>%
#   filter(is.na(eez_sovereign_iso3c)) %>%
#   mutate(eez_sovereign_iso3c = countrycode(sourcevar = eez_sovereign_name, origin = "country.name", destination = "iso3c")) %>%
#   mutate(eez_sovereign_iso3c = case_when(eez_sovereign_name == "Johnston Atoll" ~ "JTN",
#                                          eez_sovereign_name == "Palmyra Atoll" ~ "UMI",
#                                          eez_sovereign_name == "Jarvis Island" ~ "UMI",
#                                          eez_sovereign_name == "High Seas" ~ "HSX", 
#                                          eez_sovereign_name == "Saba" ~ "BES",
#                                          eez_sovereign_name == "Sint Eustatius" ~ "BES",
#                                          eez_sovereign_name == "Canary Islands" ~ "ESP",
#                                          eez_sovereign_name == "Bonaire" ~ "BES",
#                                          eez_sovereign_name == "Madeira Islands" ~ "XMI",
#                                          eez_sovereign_name == "Andaman and Nicobar Islands" ~ "IND",
#                                          eez_sovereign_name == "Howland and Baker Islands" ~ "UMI",
#                                          eez_sovereign_name == "Clipperton Island" ~ "CPT",
#                                          eez_sovereign_name == "Juan de Nova Island" ~ "ATF",
#                                          eez_sovereign_name == "Tristan da Cunha" ~ "TAA",
#                                          eez_sovereign_name == "Kerguelen Islands" ~ "ATF",
#                                          eez_sovereign_name == "Jan Mayen Island" ~ "SJM",
#                                          eez_sovereign_name == "Prince Edward Island" ~ "ZAF",
#                                          eez_sovereign_name == "Wake Island" ~ "UMI",
#                                          eez_sovereign_name == "Ascension" ~ "ASC",
#                                          eez_sovereign_name == "Chagos Archipelago" ~ "IOT",
#                                          TRUE ~ eez_sovereign_iso3c)) 
# 
# eez_lookup_fix <- eez_lookup %>%
#   rbind(fix_eez)
# 
# write.csv(eez_lookup_fix, here("data/eez_lookup_fix.csv"), row.names = FALSE)

eez_lookup_fix <- read.csv(here("data/eez_lookup_fix.csv"))


# testing_flag <- test %>%
#   distinct(flag_country_name, flag_country_iso3c) %>%
#   filter(is.na(flag_country_name)) # 0? Cool!  
# 
# testing_fao <- test %>%
#   filter(is.na(fao_major_fishing_area)) # 0 good

# eff_df_clean_fix <- eff_df_clean %>%
#   filter(is.na(eez_sovereign_iso3c)) %>%
#   dplyr::select(-eez_sovereign_iso3c) %>%
#   left_join(., fix_eez)

eff_df_clean_fin <- eff_df_clean %>%
  dplyr::select(-eez_sovereign_iso3c) %>%
  left_join(eez_lookup_fix) %>%
  mutate(sector = case_when(sector == "I" ~ "Industrial",
                            sector == "APW" ~ "Artisanal Powered",
                            sector == "UP" ~ "Artisanal Unpowered",
                            TRUE ~ sector))


## maybe save by flag country? Then we can figure out how to put it in the plot in the app...

flags <- eff_df_clean_fin %>%
  distinct(flag_country_iso3c) %>%
  collect() %>%
  pull(flag_country_iso3c) %>%
  unique()


eff_df_clean_fin <- eff_df_clean_fin %>%
  collect()

for(flag in flags){
  
  # flag = "CUB"
  
  eff_df_clean_fin %>%
    filter(flag_country_iso3c == flag) %>%
    # write_parquet(., here(glue("rousseau_data/{flag}_effort.parquet")))
    qs::qsave(., here(glue("rousseau_data/{flag}_effort.qs")))
  
}


# maybe save an "All" category? All category is just far too big to work in the Shiny...

# all_eff <- eff_df_clean_fin %>%
#   group_by(year, eez_sovereign_iso3c, eez_sovereign_name, fao_fishing_id,
#            fao_major_fishing_area, gear, length_category, f_group, sector) %>%
#   summarise(nom_active = sum(nom_active, na.rm = TRUE),
#             eff_active = sum(eff_active, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(flag_country_iso3c = "All",
#          flag_country_name = "All")


# qsave(all_eff, here("rousseau_data/All_effort.qs"))
# 
# write_parquet(all_eff, here("rousseau_data/All_effort.parquet")) # ok how do i
# # make this smaller... Ideally we would have EEZ included in this... It is just too big for shiny. 

## ok lets try to save an "All" flag category separately for each group by. So we'll
## save a file for just gear type, and one for just vessel length, etc. 

# gear df first 
gear_df <-  eff_df_clean_fin %>%
  group_by(year, eez_sovereign_iso3c, eez_sovereign_name, fao_fishing_id,
           fao_major_fishing_area, gear, sector) %>%
  summarise(nom_active = sum(nom_active, na.rm = TRUE),
            eff_active = sum(eff_active, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(flag_country_iso3c = "All",
         flag_country_name = "All")

qs::qsave(gear_df, here("rousseau_data/all_dfs/All_gear_effort.qs"))

# now length
length_df <-  eff_df_clean_fin %>%
  group_by(year, eez_sovereign_iso3c, eez_sovereign_name, fao_fishing_id,
           fao_major_fishing_area, length_category, sector) %>%
  summarise(nom_active = sum(nom_active, na.rm = TRUE),
            eff_active = sum(eff_active, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(flag_country_iso3c = "All",
         flag_country_name = "All")

qs::qsave(length_df, here("rousseau_data/all_dfs/All_length_effort.qs"))



# now fgroup
f_df <-  eff_df_clean_fin %>%
  group_by(year, eez_sovereign_iso3c, eez_sovereign_name, fao_fishing_id,
           fao_major_fishing_area, f_group, sector) %>%
  summarise(nom_active = sum(nom_active, na.rm = TRUE),
            eff_active = sum(eff_active, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(flag_country_iso3c = "All",
         flag_country_name = "All")

qs::qsave(f_df, here("rousseau_data/all_dfs/All_fgroup_effort.qs"))

# now sector
sector_df <-  eff_df_clean_fin %>%
  group_by(year, eez_sovereign_iso3c, eez_sovereign_name, fao_fishing_id,
           fao_major_fishing_area, sector, sector) %>%
  summarise(nom_active = sum(nom_active, na.rm = TRUE),
            eff_active = sum(eff_active, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(flag_country_iso3c = "All",
         flag_country_name = "All")

qs::qsave(sector_df, here("rousseau_data/all_dfs/All_sector_effort.qs"))



