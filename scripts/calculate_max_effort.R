
library(ggplot2)
library(dplyr)
library(qs)
library(rnaturalearth)  # For world map data


# Read all data files
data_files <- list.files("rf_model_data", pattern = "model_preds_1950_2017_.*\\.qs$", full.names = TRUE)

# Read total effort data for percentage calculations
total_effort_data <- qread("data/total_effort_data.qs")


# Function to read and process each file
read_data_file <- function(file_path) {
  # Extract the flag country code from the filename
  flag_code <- gsub(".*model_preds_(.+)\\.qs$", "\\1", file_path)
  
  # Read the data
  df <- qread(file_path) %>%
    filter(nom_active_fishing_hours > 0)
  
  # Return the data
  return(df)
}

# Read and combine all data files
data <- lapply(data_files, read_data_file) %>%
  bind_rows()



max_effort_nominal <- data %>%
  group_by(lon, lat, year) %>%
  summarise(nom_active_fishing_days = sum(nom_active_fishing_days, na.rm = TRUE)) %>%
  ungroup() %>%
  pull(., nom_active_fishing_days) %>%
  max(., na.rm = TRUE)

min_effort_nominal <- data %>%
  group_by(lon, lat, year) %>%
  summarise(nom_active_fishing_days = sum(nom_active_fishing_days, na.rm = TRUE)) %>%
  ungroup() %>%
  pull(., nom_active_fishing_days) %>%
  min(., na.rm = TRUE)
# ok the max nominal effort in a cell across all years is 1460531; we should set the limit of our map plots to 1.5 billion
# min nominal is 0.52

max_effort_effective <- data %>%
  group_by(lon, lat, year) %>%
  summarise(eff_active_fishing_days = sum(eff_active_fishing_days, na.rm = TRUE)) %>%
  ungroup() %>%
  pull(., eff_active_fishing_days) %>%
  max(., na.rm = TRUE)

min_effort_effective <- data %>%
  group_by(lon, lat, year) %>%
  summarise(eff_active_fishing_days = sum(eff_active_fishing_days, na.rm = TRUE)) %>%
  ungroup() %>%
  pull(., eff_active_fishing_days) %>%
  min(., na.rm = TRUE)
# ok the max nominal effort in a cell across all years is 35664829728; we should set the limit of our map plots to 40 billion
# min effective in 0.98
  