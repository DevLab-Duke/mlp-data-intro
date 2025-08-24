########################
########################
library(tidyverse)
library(here)
source(here("build_data", "mlp_functions.R"))
source(here("build_data", "constants.R"))

########################
## Create MLP data using modified mlp package functions

cat("Civic agg count: ", length(list.files(here("data","1-civic-aggregate"), pattern="[.]csv$")), "\n")
cat("RAI   agg count: ", length(list.files(here("data","1-rai-aggregate"),   pattern="[.]csv$")), "\n")


# Only remaining Civic country (add this to RAI when it's available)
country_name_list = c('Albania', 'Algeria', 'Angola', 'Armenia', 'Azerbaijan', 'Bangladesh', 'Belarus', 'Benin', 'Burkina Faso', 'Cambodia', 'Cameroon', 'Colombia', 'Costa Rica', 'DR Congo', 'Dominican Republic', 'Ecuador', 'El Salvador', 'Ethiopia', 'Georgia', 'Ghana', 'Guatemala', 'Honduras', 'Hungary', 'India', 'Indonesia', 'Jamaica', 'Kazakhstan', 'Kenya', 'Kosovo', 'Kyrgyzstan', 'Liberia', 'Macedonia', 'Malawi', 'Malaysia', 'Mali', 'Mauritania', 'Mexico', 'Moldova', 'Morocco', 'Mozambique', 'Namibia', 'Nepal', 'Nicaragua', 'Niger', 'Nigeria', 'Pakistan', 'Panama', 'Paraguay', 'Peru', 'Philippines', 'Rwanda', 'Senegal', 'Serbia', 'Solomon Islands', 'South Africa', 'South Sudan', 'Sri Lanka', 'Tanzania', 'Timor Leste', 'Tunisia', 'Turkey', 'Uganda', 'Ukraine', 'Uzbekistan', 'Zambia', 'Zimbabwe')
## Civic
# for (cc in country_name_list) {
#   cat("Civic:", cc, "\n")
#   cat("Source-level data for", cc, "\n")
#   df <- extract_civic_counts_by_source(cc, date = date_folder(cc, "civic"), use_region_filter = TRUE)
#   cat("Aggregated country-month data for", cc, "\n")
#   df <- aggregate_and_merge(cc, quiet = FALSE)

# }
for (cc in country_name_list) {
  tryCatch({
    cat("Civic:", cc, "\n")
    df <- extract_civic_counts_by_source(cc, date = date_folder(cc, "civic"),
                                         use_region_filter = TRUE)
    df <- aggregate_and_merge(cc, quiet = FALSE)
  }, error = function(e) {
    message("Civic failed for ", cc, ": ", conditionMessage(e))
  })
}

# RAI
# Already run throughgh Civic, but New RAI wasn't ready
# country_name_list = c("Armenia", "Belarus", "Hungary", "Azerbaijan", "Moldova", "Macedonia", "Turkey", "Uzbekistan", "Kyrgyzstan", "Kazakhstan", "Algeria", "Guatemala", "Bangladesh", "Cambodia", "India", "Indonesia", "Malaysia", "Nepal", "Pakistan", "Philippines", "Sri Lanka", "Timor Leste", "Angola", "Burkina Faso", "Cameroon", "DR Congo", "Ghana", "Liberia", "Malawi", "Mozambique", "Namibia", "Nigeria", "Rwanda", "South Africa", "South Sudan", "Tunisia", "Uganda", "Mexico")
# country_name_list = c('Mexico')
country_name_list = c('Albania', 'Algeria', 'Angola', 'Armenia', 'Azerbaijan', 'Bangladesh', 'Belarus', 'Benin', 'Burkina Faso', 'Cambodia', 'Cameroon', 'Colombia', 'Costa Rica', 'DR Congo', 'Dominican Republic', 'Ecuador', 'El Salvador', 'Ethiopia', 'Georgia', 'Ghana', 'Guatemala', 'Honduras', 'Hungary', 'India', 'Indonesia', 'Jamaica', 'Kazakhstan', 'Kenya', 'Kosovo', 'Kyrgyzstan', 'Liberia', 'Macedonia', 'Malawi', 'Malaysia', 'Mali', 'Mauritania', 'Mexico', 'Moldova', 'Morocco', 'Mozambique', 'Namibia', 'Nepal', 'Nicaragua', 'Niger', 'Nigeria', 'Pakistan', 'Panama', 'Paraguay', 'Peru', 'Philippines', 'Rwanda', 'Senegal', 'Serbia', 'Solomon Islands', 'South Africa', 'South Sudan', 'Sri Lanka', 'Tanzania', 'Timor Leste', 'Tunisia', 'Turkey', 'Uganda', 'Ukraine', 'Uzbekistan', 'Zambia', 'Zimbabwe')
## RAI
# for (cc in country_name_list) {
#   cat("RAI:", cc, "\n")
#   cat("Source-level data for", cc, "\n")
#   df <- extract_rai_counts_by_source_and_influencer(cc, date = date_folder(cc, "rai"), use_region_filter = TRUE)
#   cat("Aggregated country-month data for", cc, "\n")
#   df <- aggregate_and_merge_rai(cc, quiet = FALSE)
  
# }
for (cc in country_name_list) {
  tryCatch({
    cat("RAI:", cc, "\n")
    df <- extract_rai_counts_by_source_and_influencer(cc, date = date_folder(cc, "rai"),
                                                      use_region_filter = TRUE)
    df <- aggregate_and_merge_rai(cc, quiet = FALSE)
  }, error = function(e) {
    message("RAI failed for ", cc, ": ", conditionMessage(e))
  })
}


if (exists("df", inherits = FALSE)) rm(df)

########################
## Create source entries file

# Update source_entries and the legacy 1-story_counts.rda (in datastore)
cat("Source entries\n")
source_entries <- update_source_entries(use_rai = FALSE)

########################
## Combine and merge
combine_folder <- function(in_dir, out_file) {
  files <- list.files(here(in_dir), pattern="\\.csv$", full.names = TRUE)
  if (!length(files)) {
    message("No input CSVs found in ", in_dir, "; skipping write to ", out_file)
    return(invisible(NULL))
  }
  df_list <- lapply(files, readr::read_csv, show_col_types = FALSE)
  combined <- dplyr::bind_rows(df_list)
  dir.create(dirname(here(out_file)), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(combined, here(out_file))
  message("Wrote ", out_file, " (", nrow(combined), " rows)")
}

## Civic
# Get a list of all CSV files
# file_list <- list.files(here("data", "1-civic-aggregate"), pattern = "\\.csv$", full.names = TRUE)

# # Read each file into a list of data frames
# df_list <- lapply(paste0(file_list), read.csv)
# cdat <- bind_rows(df_list)

# write_csv(cdat, here("data", "final-counts", "full-civic-data.csv"))
combine_folder("data/1-civic-aggregate", "data/final-counts/full-civic-data.csv")

## RAI
# Get a list of all CSV files
# file_list <- list.files(here("data", "1-rai-aggregate"), pattern = "\\.csv$", full.names = TRUE)

# # Read each file into a list of data frames
# df_list <- lapply(paste0(file_list), read.csv)
# rdat <- bind_rows(df_list)

# write_csv(rdat, here("data", "final-counts", "full-rai-data.csv"))
combine_folder("data/1-rai-aggregate",   "data/final-counts/full-rai-data.csv")






