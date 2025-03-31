library(qs)
library(tidyverse)
library(glue)
library(here)

### read in all prediction data and save to rf_model_data folder

data_dir <- file.path("/homevol/fishingeffort/data_storage")

all_files <- list.files(file.path(data_dir, "prep/random_forest/predictions/"), full.names = TRUE)
all_files <- all_files[-101]

for(file in all_files){
  
#  file <- all_files[3]
  
  data <- qs::qread(file)
  if(nrow(data) == 0){
    next()
  }
  
  file_name <- basename(file)
  
  qs::qsave(data, here(glue("rf_model_data/{file_name}")))
  
}


all_files <- list.files(file.path(data_dir, "prep/random_forest/zenodo_data/mapped_by_flag_country/"), full.names = TRUE)

for(file in all_files){
  
  #  file <- all_files[1]
  
  data <- read.csv(file)
  
  file_name <- basename(file)
  
  write.csv(data, here(glue("mapped_by_flag_country/{file_name}")))
  
}
