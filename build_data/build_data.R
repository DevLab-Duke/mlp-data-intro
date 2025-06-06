########################
########################
library(tidyverse)
library(here)
source(here("build_data", "mlp_functions.R"))
source(here("build_data", "constants.R"))

########################
## Create MLP data using modified mlp package functions

country_name_list = c("Belarus")
for (cc in country_name_list) {
  ## Civic
  cat("Civic:", cc, "\n")
  cat("Source-level data for", cc, "\n")
  df <- extract_civic_counts_by_source(cc, date = date_folder(cc, "civic"))
  cat("Aggregated country-month data for", cc, "\n")
  df <- aggregate_and_merge(cc, quiet = FALSE)
  ## RAI
  cat("RAI:", cc, "\n")
  cat("Source-level data for", cc, "\n")
  df <- extract_rai_counts_by_source_and_influencer(cc, date = date_folder(cc, "rai"))
  cat("Aggregated country-month data for", cc, "\n")
  df <- aggregate_and_merge_rai(cc, quiet = FALSE)
  
}

rm(df)

########################
## Create source entries file

# Update source_entries and the legacy 1-story_counts.rda (in datastore)
cat("Source entries\n")
source_entries <- update_source_entries()


########################
## Combine and merge

# Get a list of all CSV files
file_list <- list.files(here("data", "0-civic-by-source"), pattern = "\\.csv$", full.names = TRUE)

# Read each file into a list of data frames
df_list <- lapply(paste0(file_list), read.csv)

cdat <- bind_rows(df_list)

data = merge(dat, cdat)

# rm(list = ls()[ls() != "dat"])





