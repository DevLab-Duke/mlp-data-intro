# Main Script to Generate All Plots
# This script coordinates the generation of all plots using the modular structure
# 
# Author: Refactored version  
# Date: 2025-01-27

library(here)

# Debug: Show where here() thinks the root is
cat("here() root directory:", here(), "\n")
cat("Looking for constants.R at:", here("build_data", "constants.R"), "\n")
cat("constants.R exists:", file.exists(here("build_data", "constants.R")), "\n")

# Source all modules
source(here("writing", "event_validation", "01_data_loading.R"))
source(here("writing", "event_validation", "02_plotting_functions.R"))
source(here("writing", "event_validation", "03_regional_plots.R"))
source(here("writing", "event_validation", "04_country_plots.R"))

#' Main function to generate all plots
#' @param output_path directory to save all plots
#' @param generate_regional logical, whether to generate regional plots
#' @param generate_country logical, whether to generate country-specific plots
main <- function(output_path = here("writing", "event_validation"),
                generate_regional = TRUE,
                generate_country = TRUE) {
  
  cat("===============================================\n")
  cat("     MLP Data Introduction Plot Generation    \n")
  cat("===============================================\n\n")
  
  start_time <- Sys.time()
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    message("Created output directory: ", output_path)
  }
  
  # Test data loading
  message("Testing data loading...")
  tryCatch({
    data_list <- load_all_data()
    message("✓ Data loading successful")
    message("  - Shock data: ", nrow(data_list$shock_data), " rows")
    message("  - Civic data: ", nrow(data_list$civic_data), " rows")
  }, error = function(e) {
    stop("✗ Data loading failed: ", e$message)
  })
  
  # Generate regional plots
  if (generate_regional) {
    message("\n=== GENERATING REGIONAL PLOTS ===")
    tryCatch({
      generate_all_regional_plots(output_path)
      message("✓ Regional plots completed")
    }, error = function(e) {
      warning("✗ Regional plots failed: ", e$message)
    })
  }
  
  # Generate country-specific plots
  if (generate_country) {
    message("\n=== GENERATING COUNTRY-SPECIFIC PLOTS ===")
    tryCatch({
      generate_all_country_plots(output_path)
      message("✓ Country-specific plots completed")
    }, error = function(e) {
      warning("✗ Country-specific plots failed: ", e$message)
    })
  }
  
  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)
  
  cat("\n===============================================\n")
  cat("              GENERATION COMPLETE             \n")
  cat("===============================================\n")
  cat("Total time:", duration, "minutes\n")
  cat("Output directory:", output_path, "\n")
  cat("Files generated:", length(list.files(output_path, pattern = "\\.png$")), "PNG files\n")
  cat("===============================================\n\n")
}

# Execute main function when script is run directly
main()
