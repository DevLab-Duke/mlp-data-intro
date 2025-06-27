# Regional Plots Module
# Generate plots grouped by region
# 
# Author: Refactored version
# Date: 2025-01-27

library(here)

# Source required modules
source(here("writing", "event_validation", "01_data_loading.R"))
source(here("writing", "event_validation", "02_plotting_functions.R"))

# Martial law shock plots removed - no longer needed

#' Generate normalized count plots with shock points for all regions and specified events
#' @param output_path directory to save plots
#' @param event_vars vector of event variables to plot
generate_regional_count_plots <- function(output_path = here("writing", "event_validation"),
                                         event_vars = c("martiallaw", "activism")) {
  
  # Load data
  data_list <- load_all_data()
  civic_data <- data_list$civic_data
  shock_data <- data_list$shock_data
  
  # Get unique regions from the data
  regions <- unique(civic_data$region)
  regions <- regions[!is.na(regions)]
  
  message("Generating normalized count plots with shock points for regions: ", paste(regions, collapse = ", "))
  message("Event variables: ", paste(event_vars, collapse = ", "))
  
  # Create plots for each region and event
  for (region_name in regions) {
    for (event_var in event_vars) {
      message("Creating ", event_var, " plot for: ", region_name)
      
      p <- create_regional_count_plot_with_shocks(
        civic_data = civic_data,
        shock_data = shock_data,
        event_var = event_var,
        region_name = region_name, 
        use_normalized = TRUE,
        date_range = c("2019-01-01", "2020-12-01")
      )
      
      # Save plot
      filename <- paste0("Normalized_", str_to_title(event_var), "_", gsub(" ", "_", region_name), ".png")
      save_plot(p, filename, output_path)
      
      message("Saved: ", filename)
    }
  }
}

#' Generate all regional plots
#' @param output_path directory to save plots
generate_all_regional_plots <- function(output_path = here("writing", "event_validation")) {
  
  message("=== Starting Regional Plot Generation ===")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    message("Created output directory: ", output_path)
  }
  
  # Generate normalized count plots with shock points for key events
  message("\n--- Generating Normalized Count Plots with Shock Points ---")
  generate_regional_count_plots(output_path, event_vars = c("martiallaw", "activism"))
  
  message("\n=== Regional Plot Generation Complete ===")
}