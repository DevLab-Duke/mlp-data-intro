# Data Loading Module for Plot Generation
# This module loads and prepares all data sources for plotting
# 
# Author: Refactored version
# Date: 2025-01-27

library(dplyr)
library(here)

# Load constants
source(here("build_data", "constants.R"))

#' Load shock data from the new data structure
#' @return data.frame with shock data including country and region information
#' @note The shock data file contains normalized event counts that represent shocks
load_shock_data <- function() {
  shock_data <- read.csv(here("data", "final-counts", "shock-civic-data.csv"), stringsAsFactors = FALSE)
  
  # Add region information using constants.R mappings
  shock_data$region <- NA
  for (region in names(country_regions)) {
    shock_data$region[shock_data$country %in% country_regions[[region]]] <- region
  }
  
  # Convert region codes to readable names
  shock_data <- shock_data %>%
    mutate(region = case_when(
      region == "EE_CA" ~ "Europe and Central Asia",
      region == "MENA" ~ "Middle East and North Africa", 
      region == "LAC" ~ "Latin America and Caribbean",
      region == "EA" ~ "East Asia and Pacific",
      region == "SSA" ~ "Sub-Saharan Africa",
      TRUE ~ region
    ))
  
  # Ensure date is in proper format
  shock_data$date <- as.Date(shock_data$date)
  
  return(shock_data)
}

#' Load civic counts data from the new data structure
#' @return data.frame with civic counts data (already includes normalized versions)
load_civic_data <- function() {
  civic_data <- read.csv(here("data", "final-counts", "full-civic-data.csv"), stringsAsFactors = FALSE)
  
  # Add region information using constants.R mappings
  civic_data$region <- NA
  for (region in names(country_regions)) {
    civic_data$region[civic_data$country %in% country_regions[[region]]] <- region
  }
  
  # Convert region codes to readable names
  civic_data <- civic_data %>%
    mutate(region = case_when(
      region == "EE_CA" ~ "Europe and Central Asia",
      region == "MENA" ~ "Middle East and North Africa", 
      region == "LAC" ~ "Latin America and Caribbean", 
      region == "EA" ~ "East Asia and Pacific",
      region == "SSA" ~ "Sub-Saharan Africa",
      TRUE ~ region
    ))
  
  # Ensure date is in proper format
  civic_data$date <- as.Date(civic_data$date)
  
  return(civic_data)
}

#' Get raw civic variables from constants.R
#' @return character vector of civic variables
get_civic_variables <- function() {
  return(civic)
}

#' Get normalized civic variables (those with Norm suffix)
#' @return character vector of normalized civic variables
get_normalized_civic_variables <- function() {
  return(paste0(civic, "Norm"))
}

#' Load variable labels from cs_vars.csv
#' @return named character vector where names are variable ids and values are labels
load_variable_labels <- function() {
  labels_data <- read.csv(here("data", "cs_vars.csv"), stringsAsFactors = FALSE)
  labels <- setNames(labels_data$name, labels_data$id)
  return(labels)
}

#' Get proper label for a variable
#' @param var_id variable identifier (e.g., "coup", "martiallaw")
#' @return character string with proper label
get_variable_label <- function(var_id) {
  labels <- load_variable_labels()
  if (var_id %in% names(labels)) {
    return(labels[[var_id]])
  } else {
    # Fallback to formatted version of the variable name
    return(str_to_title(gsub("([a-z])([A-Z])", "\\1 \\2", var_id)))
  }
}

#' Main function to load all data
#' @return list containing shock_data and civic_data
load_all_data <- function() {
  message("Loading shock data...")
  shock_data <- load_shock_data()
  
  message("Loading civic data...")
  civic_data <- load_civic_data()
  
  message("Data loading complete!")
  message(paste("Shock data:", nrow(shock_data), "rows,", ncol(shock_data), "columns"))
  message(paste("Civic data:", nrow(civic_data), "rows,", ncol(civic_data), "columns"))
  
  return(list(
    shock_data = shock_data,
    civic_data = civic_data
  ))
}