# Country-Specific Plots Module
# Generate plots for individual countries (coups, elections, etc.)
# 
# Author: Refactored version
# Date: 2025-01-27

library(here)
library(dplyr)
library(purrr)

# Source required modules
source(here("writing", "event_validation", "01_data_loading.R"))
source(here("writing", "event_validation", "02_plotting_functions.R"))

# Define countries with coups, their events, and specific date ranges (from original script)
COUP_COUNTRIES <- list(
  "Burkina Faso" = list(
    date_range = c("2021-07-01", "2024-03-31"),
    events = data.frame(
      dates = as.Date(c("2022-01-01", "2022-09-01", "2023-09-01")),
      labels = c("Coup", "Coup", "Attempt")
    )
  ),
  "DR Congo" = list(
    date_range = c("2023-11-01", "2024-11-30"),
    events = data.frame(
      dates = as.Date(c("2024-05-01")),
      labels = c("Attempt")
    )
  ),
  "Ethiopia" = list(
    date_range = c("2018-12-01", "2019-12-31"),
    events = data.frame(
      dates = as.Date(c("2019-06-01")),
      labels = c("Attempt")
    )
  ),
  "Mali" = list(
    date_range = c("2020-02-01", "2022-11-30"),
    events = data.frame(
      dates = as.Date(c("2020-08-01", "2021-05-01", "2022-05-01")),
      labels = c("Coup", "Coup", "Attempt")
    )
  ),
  "Niger" = list(
    date_range = c("2020-09-01", "2024-01-31"),
    events = data.frame(
      dates = as.Date(c("2021-03-01", "2023-07-01")),
      labels = c("Attempt", "Coup")
    )
  ),
  "Peru" = list(
    date_range = c("2022-06-01", "2023-06-30"),
    events = data.frame(
      dates = as.Date(c("2022-12-01")),
      labels = c("Self-Coup")
    )
  ),
  "Tunisia" = list(
    date_range = c("2021-01-01", "2022-01-31"),
    events = data.frame(
      dates = as.Date(c("2021-07-01")),
      labels = c("Self-Coup")
    )
  ),
  "Turkey" = list(
    date_range = c("2016-01-01", "2017-01-31"),
    events = data.frame(
      dates = as.Date(c("2016-07-01")),
      labels = c("Attempt")
    )
  ),
  "Zimbabwe" = list(
    date_range = c("2017-05-01", "2018-05-31"),
    events = data.frame(
      dates = as.Date(c("2017-11-01")),
      labels = c("Attempt")
    )
  )
)

# Define countries with elections BY REGION and their specific configurations (from original script)
# Each region will get its own combined election plot
ELECTION_REGIONS <- list(
  "Latin America and Caribbean" = list(
    "Colombia" = list(
      date_range = c("2021-01-01", "2023-01-01"),
      events = data.frame(
        dates = as.Date(c("2022-03-01", "2022-05-01", "2022-06-01")),
        labels = c("Congressional Elections", "Presidential Election (1st round)", "Presidential Election (Run-off)")
      )
    ),
    "Dominican Republic" = list(
      date_range = c("2023-07-01", "2024-07-01"),
      events = data.frame(
        dates = as.Date(c("2023-10-01", "2024-02-01", "2024-05-01")),
        labels = c("Primaries", "Municipal Elections", "General Elections")
      )
    ),
    "Ecuador" = list(
      date_range = c("2023-01-01", "2024-01-01"),
      events = data.frame(
        dates = as.Date(c("2023-05-01", "2023-08-01", "2023-10-01")),
        labels = c("Extraordinary Election Announcement", "Presidential Election (1st round)", "Presidential Election (Run-off)")
      )
    ),
    "El Salvador" = list(
      date_range = c("2023-01-01", "2024-07-01"),
      events = data.frame(
        dates = as.Date(c("2023-07-01", "2023-10-01", "2024-02-01", "2024-03-01")),
        labels = c("Primaries", "Campaign Begins", "General Elections", "Municipal Elections")
      )
    ),
    "Guatemala" = list(
      date_range = c("2023-01-01", "2024-01-01"),
      events = data.frame(
        dates = as.Date(c("2023-01-01", "2023-06-01", "2023-08-01")),
        labels = c("Election Announcement", "General Elections", "Run-off Presidential Elections")
      )
    ),
    "Honduras" = list(
      date_range = c("2020-11-01", "2022-02-01"),
      events = data.frame(
        dates = as.Date(c("2021-03-01", "2021-11-01")),
        labels = c("Primaries", "General Elections")
      )
    ),
    "Jamaica" = list(
      date_range = c("2020-06-01", "2021-01-01"),
      events = data.frame(
        dates = as.Date(c("2020-08-01", "2020-09-01")),
        labels = c("Parliament Dissolved", "General Elections")
      )
    ),
    "Nicaragua" = list(
      date_range = c("2020-08-01", "2022-04-01"),
      events = data.frame(
        dates = as.Date(c("2021-04-01", "2021-11-01")),
        labels = c("Formation of New Electoral Body", "General Elections")
      )
    ),
    "Paraguay" = list(
      date_range = c("2022-07-01", "2023-11-01"),
      events = data.frame(
        dates = as.Date(c("2022-12-01", "2023-04-01")),
        labels = c("Primaries", "General Elections")
      )
    ),
    "Peru" = list(
      date_range = c("2020-08-01", "2021-11-01"),
      events = data.frame(
        dates = as.Date(c("2021-04-01", "2021-06-01")),
        labels = c("General Elections", "Run-off Election")
      )
    )
  ),
  "East Asia and Pacific" = list(
    "Cambodia" = list(
      date_range = c("2023-01-01", "2024-01-01"),
      events = data.frame(
        dates = as.Date(c("2023-07-01")),
        labels = c("General Elections")
      )
    ),
    "Indonesia" = list(
      date_range = c("2023-07-01", "2024-07-01"),
      events = data.frame(
        dates = as.Date(c("2024-02-01")),
        labels = c("General Elections")
      )
    ),
    "Malaysia" = list(
      date_range = c("2022-02-01", "2023-02-01"),
      events = data.frame(
        dates = as.Date(c("2022-11-01")),
        labels = c("General Elections")
      )
    ),
    "Philippines" = list(
      date_range = c("2021-10-01", "2022-10-01"),
      events = data.frame(
        dates = as.Date(c("2022-05-01")),
        labels = c("General Elections")
      )
    ),
    "Timor Leste" = list(
      date_range = c("2022-01-01", "2023-01-01"),
      events = data.frame(
        dates = as.Date(c("2022-03-01", "2022-04-01")),
        labels = c("Presidential Election (1st round)", "Presidential Election (Run-off)")
      )
    ),
    "Solomon Islands" = list(
      date_range = c("2024-01-01", "2024-07-01"),
      events = data.frame(
        dates = as.Date(c("2024-04-01")),
        labels = c("General Elections")
      )
    )
  )
  # Add other regions as needed based on original script
)

#' Generate coup analysis plots for specified countries
#' @param output_path directory to save plots
#' @param countries list of countries with coup dates (uses COUP_COUNTRIES by default)
generate_coup_plots <- function(output_path = here("writing", "event_validation"),
                               countries = COUP_COUNTRIES) {
  
  # Load data
  data_list <- load_all_data()
  shock_data <- data_list$shock_data
  civic_data <- data_list$civic_data
  
  message("Generating coup plots for countries: ", paste(names(countries), collapse = ", "))
  
  # Create plots for each country
  coup_plots <- list()
  
  for (country_name in names(countries)) {
    message("Creating coup plot for: ", country_name)
    
    # Check if country exists in data
    if (!country_name %in% shock_data$country || !country_name %in% civic_data$country) {
      warning("Country '", country_name, "' not found in data. Skipping.")
      next
    }
    
    # Get country-specific configuration
    country_config <- countries[[country_name]]
    
    p <- create_country_coup_plot(
      shock_data = shock_data,
      civic_data = civic_data,
      country_name = country_name,
      event_var = "coup",
      date_range = country_config$date_range,
      coup_events = country_config$events
    )
    
    coup_plots[[country_name]] <- p
    
    # Save individual plot
    filename <- paste0("Coup_", gsub(" ", "_", country_name), ".png")
    save_plot(p, filename, output_path, width = 10, height = 8)
    
    message("Saved: ", filename)
  }
  
  return(coup_plots)
}

#' Generate combined coup plot (all countries in one grid)
#' @param output_path directory to save plots
#' @param countries list of countries with coup configurations
generate_combined_coup_plot <- function(output_path = here("writing", "event_validation"),
                                       countries = COUP_COUNTRIES) {
  
  # Load data
  data_list <- load_all_data()
  shock_data <- data_list$shock_data
  civic_data <- data_list$civic_data
  
  message("Generating combined coup plot for all countries...")
  
  # Create individual plots for each country
  coup_plots <- list()
  
  for (country_name in names(countries)) {
    if (!country_name %in% shock_data$country || !country_name %in% civic_data$country) {
      warning("Country '", country_name, "' not found in data. Skipping.")
      next
    }
    
    # Get country-specific configuration
    country_config <- countries[[country_name]]
    
    p <- create_country_coup_plot(
      shock_data = shock_data,
      civic_data = civic_data,
      country_name = country_name,
      event_var = "coup",
      date_range = country_config$date_range,
      coup_events = country_config$events
    )
    
    coup_plots[[country_name]] <- p
  }
  
  # Combine all plots into a single grid
  if (length(coup_plots) > 0) {
    grid_plot <- wrap_plots(coup_plots, ncol = 2) +
      plot_layout(guides = 'collect') & theme(legend.position = "bottom")
    
    # Save combined plot
    filename <- "Combined_Coup_Plots.png"
    save_plot(grid_plot, filename, output_path, width = 20, height = 16)
    
    message("Saved combined plot: ", filename)
    return(grid_plot)
  } else {
    warning("No coup plots generated for combined plot")
    return(NULL)
  }
}

#' Generate election plots for countries with specific election data
#' @param output_path directory to save plots
#' @param election_regions list of regions with their country election configurations
generate_election_plots <- function(output_path = here("writing", "event_validation"),
                                   election_regions = ELECTION_REGIONS) {
  
  # Load data
  data_list <- load_all_data()
  shock_data <- data_list$shock_data
  civic_data <- data_list$civic_data
  
  message("Generating election plots for countries with specific election data...")
  
  # Create individual plots for each country with election data
  all_election_plots <- list()
  
  # Process each region
  for (region_name in names(election_regions)) {
    region_countries <- election_regions[[region_name]]
    region_plots <- list()
    
    message("\nProcessing region: ", region_name)
    
    for (country_name in names(region_countries)) {
      if (!country_name %in% shock_data$country || !country_name %in% civic_data$country) {
        warning("Country '", country_name, "' not found in data. Skipping.")
        next
      }
      
      message("Creating election plot for: ", country_name)
      
      # Get country-specific configuration
      country_config <- region_countries[[country_name]]
      
      p <- create_election_plot(
        shock_data = shock_data,
        civic_data = civic_data,
        country_name = country_name,
        date_range = country_config$date_range,
        election_events = country_config$events
      )
      
      region_plots[[country_name]] <- p
      all_election_plots[[country_name]] <- p
      
      # Save individual plot
      filename <- paste0("Election_", gsub(" ", "_", country_name), ".png")
      save_plot(p, filename, output_path, width = 10, height = 8)
      
      message("Saved: ", filename)
    }
    
    # Create combined plot for this region
    if (length(region_plots) > 0) {
      grid_plot <- wrap_plots(region_plots, ncol = 3)
      
      # Save regional combined plot
      region_clean <- gsub(" ", "_", region_name)
      region_filename <- paste0("Combined_elections_", 
                               case_when(
                                 region_name == "Latin America and Caribbean" ~ "LAC",
                                 region_name == "East Asia and Pacific" ~ "EAP",
                                 region_name == "Europe and Central Asia" ~ "ECA",
                                 region_name == "Sub-Saharan Africa" ~ "SSA",
                                 region_name == "South Asia" ~ "SA",
                                 region_name == "Middle East and North Africa" ~ "MENA",
                                 TRUE ~ region_clean
                               ), ".png")
      
      save_plot(grid_plot, region_filename, output_path, width = 20, height = 16)
      message("Saved combined plot: ", region_filename)
    }
  }
  
  return(all_election_plots)
}

# Note: This function has been replaced by the regional processing in generate_election_plots()

#' Generate Guatemala 2015 specific analysis (from original script)
#' @param output_path directory to save plots
generate_guatemala_2015_plot <- function(output_path = here("writing", "event_validation")) {
  
  # Load data
  data_list <- load_all_data()
  civic_data <- data_list$civic_data
  
  message("Creating Guatemala 2015 analysis plot")
  
  # Filter for Guatemala
  guatemala_data <- civic_data %>% filter(country == "Guatemala")
  
  if (nrow(guatemala_data) == 0) {
    warning("Guatemala data not found")
    return(NULL)
  }
  
  # Create specific plot for Guatemala 2015 period
  p <- ggplot(guatemala_data, aes(x = date)) +
    geom_line(aes(y = corruptionNorm), color = "red", alpha = 0.8) +
    geom_line(aes(y = protestNorm), color = "blue", alpha = 0.8) +
    geom_line(aes(y = electionactivityNorm), color = "green", alpha = 0.8) +
    labs(title = "Guatemala 2015: Corruption, Protests, and Elections",
         x = "Date",
         y = "Normalized Counts") +
    scale_x_date(
      limits = as.Date(c("2014-01-01", "2016-12-01")),
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # Add legend
    annotate("text", x = as.Date("2015-01-01"), y = 0.15, 
             label = "Red: Corruption\nBlue: Protests\nGreen: Elections", 
             size = 3, hjust = 0)
  
  # Save plot
  filename <- "Guatemala_2015_Analysis.png"
  save_plot(p, filename, output_path)
  
  message("Saved: ", filename)
  return(p)
}

#' Generate Guatemala 2023 combined plots (4 event types: election activity, election irregularities, legal action, protest)
#' @param output_path directory to save plots
generate_guatemala_2023_plots <- function(output_path = here("writing", "event_validation")) {
  
  # Load data
  data_list <- load_all_data()
  shock_data <- data_list$shock_data
  civic_data <- data_list$civic_data
  
  message("Creating Guatemala 2023 combined plots...")
  
  # Filter for Guatemala
  guatemala_civic <- civic_data %>% filter(country == "Guatemala")
  guatemala_shock <- shock_data %>% filter(country == "Guatemala")
  
  if (nrow(guatemala_civic) == 0) {
    warning("Guatemala data not found")
    return(NULL)
  }
  
  # Merge data
  guatemala_merged <- guatemala_civic %>%
    left_join(guatemala_shock %>% select(country, date, 
                                        electionactivityNorm, electionirregularitiesNorm, 
                                        coupNorm, protestNorm), 
              by = c("country", "date"), suffix = c("", ".shock"))
  
  # Define election timeline for 2023
  vertical_lines <- data.frame(
    dates = as.Date(c("2023-01-01", "2023-06-01", "2023-08-01", "2024-01-01")),
    labels = c("Election Announcement", "General Elections", "Run-off Presidential Elections", "Inauguration")
  )
  
  # Date range for Guatemala 2023 analysis
  date_range <- c("2022-07-01", "2024-03-01")
  
  # Plot 1: Election Activity
  p1 <- ggplot(guatemala_merged, aes(x = date, y = electionactivityNorm * 100)) +
    geom_line() +
    geom_point(data = guatemala_merged %>% filter(electionactivityNorm.shock > 0),
               aes(x = date, y = electionactivityNorm * 100),
               color = "red", size = 2) +
    geom_vline(data = vertical_lines, 
               aes(xintercept = dates, color = labels), 
               linetype = "dashed", linewidth = 0.8) +
    labs(title = "Share of Articles about Electoral Activity in Guatemala",
         x = "Date",
         y = "% of Articles",
         color = "") +
    theme_minimal() +
    scale_x_date(
      limits = as.Date(date_range),
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          legend.position = "bottom") +
    scale_y_continuous(labels = scales::label_percent(scale = 1))
  
  # Plot 2: Election Irregularities
  p2 <- ggplot(guatemala_merged, aes(x = date, y = electionirregularitiesNorm * 100)) +
    geom_line() +
    geom_point(data = guatemala_merged %>% filter(electionirregularitiesNorm.shock > 0),
               aes(x = date, y = electionirregularitiesNorm * 100),
               color = "red", size = 2) +
    geom_vline(data = vertical_lines, 
               aes(xintercept = dates, color = labels), 
               linetype = "dashed", linewidth = 0.8) +
    labs(title = "Share of Articles about Election Irregularities in Guatemala",
         x = "Date",
         y = "% of Articles",
         color = "") +
    theme_minimal() +
    scale_x_date(
      limits = as.Date(date_range),
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          legend.position = "bottom") +
    scale_y_continuous(labels = scales::label_percent(scale = 1))
  
  # Plot 3: Legal Action (using coup variable as in original)
  p3 <- ggplot(guatemala_merged, aes(x = date, y = coupNorm * 100)) +
    geom_line() +
    geom_point(data = guatemala_merged %>% filter(coupNorm.shock > 0),
               aes(x = date, y = coupNorm * 100),
               color = "red", size = 2) +
    geom_vline(data = vertical_lines, 
               aes(xintercept = dates, color = labels), 
               linetype = "dashed", linewidth = 0.8) +
    labs(title = "Share of Articles about Legal Action in Guatemala",
         x = "Date",
         y = "% of Articles",
         color = "") +
    theme_minimal() +
    scale_x_date(
      limits = as.Date(date_range),
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          legend.position = "bottom") +
    scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 1))
  
  # Plot 4: Protest
  p4 <- ggplot(guatemala_merged, aes(x = date, y = protestNorm * 100)) +
    geom_line() +
    geom_point(data = guatemala_merged %>% filter(protestNorm.shock > 0),
               aes(x = date, y = protestNorm * 100),
               color = "red", size = 2) +
    geom_vline(data = vertical_lines, 
               aes(xintercept = dates, color = labels), 
               linetype = "dashed", linewidth = 0.8) +
    labs(title = "Share of Articles about Protest in Guatemala",
         x = "Date",
         y = "% of Articles",
         color = "") +
    theme_minimal() +
    scale_x_date(
      limits = as.Date(date_range),
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 18, face = "bold"),
          legend.position = "bottom") +
    scale_y_continuous(labels = scales::label_percent(scale = 1))
  
  # Combine all plots into a single grid
  plots_GTM <- list(p1, p2, p3, p4)
  grid_plot <- wrap_plots(plots_GTM, ncol = 2) +
    plot_layout(guides = 'collect') & theme(legend.position = "bottom")
  
  # Save combined plot
  filename <- "Combined_GTM2023_Plots.png"
  save_plot(grid_plot, filename, output_path, width = 20, height = 16)
  
  message("Saved combined Guatemala 2023 plot: ", filename)
  return(grid_plot)
}

#' Generate all country-specific plots
#' @param output_path directory to save plots
generate_all_country_plots <- function(output_path = here("writing", "event_validation")) {
  
  message("=== Starting Country-Specific Plot Generation ===")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    message("Created output directory: ", output_path)
  }
  
  # Generate individual coup plots
  message("\n--- Generating Individual Coup Analysis Plots ---")
  coup_plots <- generate_coup_plots(output_path)
  
  # Generate combined coup plot
  message("\n--- Generating Combined Coup Plot ---")
  combined_coup <- generate_combined_coup_plot(output_path)
  
  # Generate election plots
  message("\n--- Generating Election Plots ---")
  election_plots <- generate_election_plots(output_path)
  
  # Generate Guatemala 2015 analysis
  message("\n--- Generating Guatemala 2015 Analysis ---")
  guatemala_plot <- generate_guatemala_2015_plot(output_path)
  
  # Generate Guatemala 2023 combined plots
  message("\n--- Generating Guatemala 2023 Combined Plots ---")
  guatemala_2023_plot <- generate_guatemala_2023_plots(output_path)
  
  message("\n=== Country-Specific Plot Generation Complete ===")
  
  return(list(
    coup_plots = coup_plots,
    combined_coup = combined_coup,
    election_plots = election_plots,
    guatemala_plot = guatemala_plot,
    guatemala_2023_plot = guatemala_2023_plot
  ))
}