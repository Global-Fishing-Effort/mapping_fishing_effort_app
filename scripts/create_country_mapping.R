# Script to create country names mapping from rousseau_data files
# This loops through all files in rousseau_data and extracts unique flag_country_iso3c and flag_country_name values

library(here)
library(glue)
library(qs)
library(dplyr)

# Initialize empty data frame to store country mappings
flags_df <- data.frame()

# Get list of files in rousseau_data directory
files <- list.files(here("rousseau_data"))

cat("Processing", length(files), "files from rousseau_data directory...\n")

# Loop through each file
for(file in files) {
  
 # file <- files[[1]]
  
  cat("Processing file:", file, "\n")
  
  # Read the data file
  df <- qs::qread(here(glue("rousseau_data/{file}")))
  
  # Extract unique flag country information
  if("flag_country_iso3c" %in% names(df) && "flag_country_name" %in% names(df)) {
    
    # Get unique combinations of iso3c and country name
    unique_countries <- unique(df[, c("flag_country_iso3c", "flag_country_name")])
    
    # Add to our master data frame
    flags_df <- rbind(flags_df, unique_countries)
    
  } else {
    cat("Warning: flag_country_iso3c or flag_country_name columns not found in", file, "\n")
  }
}

# Get final unique combinations
flags_df <- unique(flags_df)

flags_df <- flags_df %>%
  mutate(flag_country_name = case_when(
    flag_country_iso3c == "COG" ~ "Republic of the Congo",
    flag_country_iso3c == "COD" ~ "Democratic Republic of the Congo",
    TRUE ~ flag_country_name
  ))

cat("\nFound", nrow(flags_df), "unique country mappings:\n")
print(flags_df)

# Convert to named vector for consistency with original format
country_names <- setNames(flags_df$flag_country_name, flags_df$flag_country_iso3c)

# Save the mapping to a .qs file
qs::qsave(country_names, "data/country_names_mapping.qs")

cat("\nCountry names mapping saved to data/country_names_mapping.qs\n")
cat("Total countries:", length(country_names), "\n")


# 
# country_names <- qread(here("data/country_names_mapping.qs"))
