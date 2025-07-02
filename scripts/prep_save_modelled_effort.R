library(qs)
library(tidyverse)
library(glue)
library(here)

### read in all prediction data and save to rf_model_data folder

data_dir <- file.path("/homevol/fishingeffort/data_storage")

all_files <- list.files(file.path(data_dir, "prep/random_forest/zenodo_data/mapped_by_flag_country/"), full.names = TRUE)

for(file in all_files){
  
#  file <- all_files[3]
  
  data <- read.csv(file)
  if(nrow(data) == 0){
    next()
  }
  
  
  # also figure out color palette here? https://www.nature.com/articles/s41597-023-02824-6/figures/7
  
  # lowest: #FFFFFF
  #EFF3FE
  #CADBEE
  #A8C9E0
  #E8F4A2
  #F1B16D
  # highest: #C54B53
  
  file_name <- basename(file)
  
  qs::qsave(data, here(glue("rf_model_data/{file_name}")))
  
}

test <- qs::qread(here("rf_model_data/model_preds_1950_2017_AGO.qs"))
