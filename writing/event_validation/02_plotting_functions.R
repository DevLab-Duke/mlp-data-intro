# Plotting Functions Module
# Reusable functions for creating various types of plots
# 
# Author: Refactored version
# Date: 2025-01-27

library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)
library(stringr)

#' Create regional shock plots for a specific event type
#' @param data shock data (contains normalized variables that represent shocks)
#' @param event_var variable name for the event (e.g., "martiallaw")
#' @param region_name name of the region to plot
#' @param date_range vector of two dates for x-axis limits
#' @param covid_line logical, whether to add COVID-19 reference line
#' @return ggplot object
create_regional_shock_plot <- function(data, event_var, region_name, 
                                     date_range = c("2019-01-01", "2020-12-01"),
                                     covid_line = TRUE) {
  
  # Filter data for the region
  region_data <- data %>% filter(region == region_name)
  
  # The shock data contains normalized columns that represent shock intensity
  shock_var <- paste0(event_var, "Norm")
  
  # Create plot
  p <- ggplot(region_data, aes(x = date, y = .data[[shock_var]])) +
    geom_line() +
    facet_wrap(~country, scales = "free_x") +
    labs(title = paste(str_to_title(gsub("([a-z])([A-Z])", "\\1 \\2", event_var)), "Shocks Over Time -", region_name),
         x = "Date",
         y = paste("Number of", str_to_title(gsub("([a-z])([A-Z])", "\\1 \\2", event_var)), "Shocks")) +
    theme_minimal() +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
    scale_x_date(
      limits = as.Date(date_range),
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  
  # Add COVID reference line if requested
  if (covid_line) {
    p <- p + geom_vline(xintercept = as.Date("2020-03-01"), 
                       linetype = "dashed", color = "red")
  }
  
  return(p)
}

#' Create regional normalized count plots for a specific event type
#' @param data civic data with normalized variables
#' @param event_var variable name for the event (e.g., "martiallaw")
#' @param region_name name of the region to plot
#' @param use_normalized logical, whether to use normalized version
#' @param date_range vector of two dates for x-axis limits
#' @return ggplot object
create_regional_count_plot <- function(data, event_var, region_name,
                                     use_normalized = TRUE,
                                     date_range = c("2019-01-01", "2020-12-01")) {
  
  # Filter data for the region
  region_data <- data %>% filter(region == region_name)
  
  # Choose variable (normalized or raw)
  plot_var <- ifelse(use_normalized, paste0(event_var, "Norm"), event_var)
  y_label <- ifelse(use_normalized, 
                   paste("Normalized", str_to_title(gsub("([a-z])([A-Z])", "\\1 \\2", event_var)), "Counts"),
                   paste(str_to_title(gsub("([a-z])([A-Z])", "\\1 \\2", event_var)), "Counts"))
  
  # Create plot
  p <- ggplot(region_data, aes(x = date, y = .data[[plot_var]])) +
    geom_line() +
    facet_wrap(~country, scales = "free_x") +
    labs(title = paste(y_label, "Over Time -", region_name),
         x = "Date",
         y = y_label) +
    theme_minimal() +
    scale_x_date(
      limits = as.Date(date_range),
      date_breaks = "3 months", 
      date_labels = "%b %Y"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  
  # Format y-axis for normalized data
  if (use_normalized) {
    p <- p + scale_y_continuous(labels = scales::label_percent(scale = 1))
  }
  
  return(p)
}

#' Create individual country plot for coup analysis (matches original script structure)
#' @param shock_data shock data (contains normalized shock values)
#' @param civic_data civic data (contains raw counts and normalized versions)
#' @param country_name name of the country
#' @param event_var event variable (default "coup")
#' @param date_range vector of two dates for x-axis limits
#' @param coup_events data.frame with columns: dates, labels (for coup vs attempt)
#' @return ggplot object
create_country_coup_plot <- function(shock_data, civic_data, country_name, 
                                   event_var = "coup",
                                   date_range = c("2020-02-01", "2024-03-31"),
                                   coup_events = NULL) {
  
  # Filter data for country
  shock_country <- shock_data %>% filter(country == country_name)
  civic_country <- civic_data %>% filter(country == country_name)
  
  # Merge the data to match original script structure
  merged_data <- civic_country %>%
    left_join(shock_country %>% select(country, date, all_of(paste0(event_var, "Norm"))), 
              by = c("country", "date"), suffix = c("", ".shock"))
  
  # Variable names
  civic_norm_var <- paste0(event_var, "Norm")
  shock_var <- paste0(event_var, "Norm.shock")
  raw_shock_var <- event_var
  
  # Create base plot with normalized civic data as line (converted to percentage)
  p <- ggplot(merged_data, aes(x = date, y = .data[[civic_norm_var]] * 100)) +
    geom_line() +
    # Add points where shocks occur (where shock value == 1 or > 0)
    geom_point(data = merged_data %>% filter(.data[[shock_var]] > 0),
               aes(x = date, y = .data[[civic_norm_var]] * 100),
               color = "red", size = 2) +
    labs(title = paste("Share of Articles about Coups in", country_name),
         x = "Date",
         y = "% of Articles",
         color = "") +
    theme_minimal() +
    scale_x_date(
      limits = as.Date(date_range),
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
    scale_y_continuous(labels = scales::label_percent(scale = 1))
  
  # Add vertical lines for coup events if provided
  if (!is.null(coup_events)) {
    p <- p + 
      geom_vline(data = coup_events, 
                 aes(xintercept = dates, color = labels), 
                 linetype = "dashed", linewidth = 0.8) +
      scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red", "Self-Coup" = "orange"))
  }
  
  return(p)
}

#' Create election plot for a specific country (matches original structure)
#' @param shock_data shock data
#' @param civic_data civic data
#' @param country_name name of the country
#' @param date_range vector of two dates for x-axis limits
#' @param election_events data.frame with election dates and labels
#' @return ggplot object
create_election_plot <- function(shock_data, civic_data, country_name,
                                date_range = c("2021-01-01", "2023-01-01"),
                                election_events = NULL) {
  
  # Filter data for country
  shock_country <- shock_data %>% filter(country == country_name)
  civic_country <- civic_data %>% filter(country == country_name)
  
  # Merge the data to match original script structure
  merged_data <- civic_country %>%
    left_join(shock_country %>% select(country, date, electionactivityNorm), 
              by = c("country", "date"), suffix = c("", ".shock"))
  
  # Create base plot with normalized civic data as line (converted to percentage)
  p <- ggplot(merged_data, aes(x = date, y = electionactivityNorm * 100)) +
    geom_line() +
    # Add points where shocks occur
    geom_point(data = merged_data %>% filter(`electionactivityNorm.shock` > 0),
               aes(x = date, y = electionactivityNorm * 100),
               color = "red", size = 2) +
    labs(title = country_name,
         x = "Date", 
         y = "% of Articles",
         color = "") +
    theme_minimal() +
    scale_x_date(
      limits = as.Date(date_range),
      date_breaks = "2 months",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold"))
  
  # Add vertical lines for election events if provided
  if (!is.null(election_events)) {
    p <- p + 
      geom_vline(data = election_events, 
                 aes(xintercept = dates, color = labels), 
                 linetype = "dashed", linewidth = 0.8) +
      theme(legend.position = "bottom")  # Show legend for individual plots
  }
  
  return(p)
}

#' Save plot with standardized settings
#' @param plot ggplot object
#' @param filename filename for the output
#' @param output_path directory to save the plot
#' @param width plot width in inches
#' @param height plot height in inches
#' @param dpi resolution
save_plot <- function(plot, filename, output_path, width = 12, height = 10, dpi = 300) {
  ggsave(filename = file.path(output_path, filename), 
         plot = plot, 
         device = "png", 
         width = width, 
         height = height, 
         dpi = dpi)
}
#' Create regional normalized count plots with shock points overlaid
#' @param civic_data civic data with normalized variables
#' @param shock_data shock data with shock detection results
#' @param event_var variable name for the event (e.g., "martiallaw")
#' @param region_name name of the region to plot
#' @param use_normalized logical, whether to use normalized version
#' @param date_range vector of two dates for x-axis limits
#' @return ggplot object
create_regional_count_plot_with_shocks <- function(civic_data, shock_data, event_var, region_name,
                                                  use_normalized = TRUE,
                                                  date_range = c("2019-01-01", "2020-12-01")) {
  
  # Filter data for the region
  civic_region <- civic_data %>% filter(region == region_name)
  shock_region <- shock_data %>% filter(region == region_name)
  
  # Merge civic and shock data
  merged_data <- civic_region %>%
    left_join(shock_region %>% select(country, date, all_of(paste0(event_var, "Norm"))), 
              by = c("country", "date"), suffix = c("", ".shock"))
  
  # Choose variable (normalized or raw)
  civic_var <- ifelse(use_normalized, paste0(event_var, "Norm"), event_var)
  shock_var <- paste0(event_var, "Norm.shock")
  y_label <- ifelse(use_normalized, 
                   paste("Normalized", str_to_title(gsub("([a-z])([A-Z])", "\\1 \\2", event_var)), "Counts"),
                   paste(str_to_title(gsub("([a-z])([A-Z])", "\\1 \\2", event_var)), "Counts"))
  
  # Create base plot with civic data line
  p <- ggplot(merged_data, aes(x = date, y = .data[[civic_var]])) +
    geom_line() +
    # Add shock points where shock detection occurred
    geom_point(data = merged_data %>% filter(.data[[shock_var]] > 0),
               aes(x = date, y = .data[[civic_var]]),
               color = "red", size = 2) +
    facet_wrap(~country, scales = "free_x") +
    labs(title = paste(y_label, "Over Time (with Shock Points) -", region_name),
         x = "Date",
         y = y_label) +
    theme_minimal() +
    scale_x_date(
      limits = as.Date(date_range),
      date_breaks = "3 months", 
      date_labels = "%b %Y"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  
  # Format y-axis for normalized data
  if (use_normalized) {
    p <- p + scale_y_continuous(labels = scales::label_percent(scale = 1))
  }
  
  return(p)
}
