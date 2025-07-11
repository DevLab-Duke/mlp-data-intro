########################
########################
library(tidyverse)
library(here)
source(here("build_data", "mlp_functions.R"))
source(here("build_data", "constants.R"))

########################
## Create MLP data using modified mlp package functions

# Only remaining Civic country (add this to RAI when it's available)
country_name_list = c("Mexico") # SSA

## Civic
for (cc in country_name_list) {
  cat("Civic:", cc, "\n")
  cat("Source-level data for", cc, "\n")
  df <- extract_civic_counts_by_source(cc, date = date_folder(cc, "civic"), use_region_filter = TRUE)
  cat("Aggregated country-month data for", cc, "\n")
  df <- aggregate_and_merge(cc, quiet = FALSE)

}

# RAI
# Already run throughgh Civic, but New RAI wasn't ready
country_name_list = c("Armenia", "Belarus", "Hungary", "Azerbaijan", "Moldova", "Macedonia", "Turkey", "Uzbekistan", "Kyrgyzstan", "Kazakhstan", "Algeria", "Guatemala", "Bangladesh", "Cambodia", "India", "Indonesia", "Malaysia", "Nepal", "Pakistan", "Philippines", "Sri Lanka", "Timor Leste", "Angola", "Burkina Faso", "Cameroon", "DR Congo", "Ghana", "Liberia", "Malawi", "Mozambique", "Namibia", "Nigeria", "Rwanda", "South Africa", "South Sudan", "Tunisia", "Uganda")

## RAI
for (cc in country_name_list) {
  cat("RAI:", cc, "\n")
  cat("Source-level data for", cc, "\n")
  df <- extract_rai_counts_by_source_and_influencer(cc, date = date_folder(cc, "rai"), use_region_filter = TRUE)
  cat("Aggregated country-month data for", cc, "\n")
  df <- aggregate_and_merge_rai(cc, quiet = FALSE)
  
}


rm(df)

########################
## Create source entries file

# Update source_entries and the legacy 1-story_counts.rda (in datastore)
cat("Source entries\n")
source_entries <- update_source_entries(use_rai = FALSE)

########################
## Combine and merge

## Civic
# Get a list of all CSV files
file_list <- list.files(here("data", "1-civic-aggregate"), pattern = "\\.csv$", full.names = TRUE)

# Read each file into a list of data frames
df_list <- lapply(paste0(file_list), read.csv)
cdat <- bind_rows(df_list)

write_csv(cdat, here("data", "final-counts", "full-civic-data.csv"))

## RAI
# Get a list of all CSV files
file_list <- list.files(here("data", "1-rai-aggregate"), pattern = "\\.csv$", full.names = TRUE)

# Read each file into a list of data frames
df_list <- lapply(paste0(file_list), read.csv)
rdat <- bind_rows(df_list)

write_csv(rdat, here("data", "final-counts", "full-rai-data.csv"))






