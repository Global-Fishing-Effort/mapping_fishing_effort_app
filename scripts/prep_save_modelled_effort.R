library(qs)
library(tidyverse)
library(glue)

### read in all prediction data and save to rf_model_data folder

data_dir <- file.path("/homevol/fishingeffort/data_storage")

all_files <- list.files(file.path(data_dir, "prep/random_forest/predictions/"), full.names = TRUE)
all_files <- all_files[-101]

for(file in all_files){
  
#  file <- all_files[116]
  
  data <- qs::qread(file)
  if(nrow(data) == 0){
    next()
  }
  
  file_name <- basename(file)
  
  qs::qsave(data, here(glue("rf_model_data/{file_name}")))
  
}
