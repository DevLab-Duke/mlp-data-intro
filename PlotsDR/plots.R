# This file generates the following plots:
# 1. A grid of plots per region showing, for each country, shocks and normalized counts of martiallaw before and after covid
# 2. Plots of normalized counts and shocks of coup for every country in the ML4P data that had a coup, attempted coup or self-coup since 2017.

# Author: Diego
# Date: December 19, 2024
# NOTE: you MUST update the paths. The original file is in the ML4P Dropbox.

rm(list = ls())
set.seed(1992)
options(scipen = 100)

# Load necessary library
library(dplyr)
library(ggplot2)

# Outupt path
output_path <- '/Users/diegoromero/Dropbox/ML for Peace/Side Projects/Plots for MLP/PLOTS'

# Set the path to the folder containing the CSV files
folder_path <- "/Users/diegoromero/Documents/GitHub/mlp-data-intro/data/shocks"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Import and process each CSV file
data_list <- lapply(csv_files, function(file) {
  # Read the CSV file
  data <- read.csv(file)
  # Extract the file name without the extension
  country_name <- tools::file_path_sans_ext(basename(file))
  # Add a new column 'country' with the file name
  data$country <- country_name
  return(data)
})

# Stack all dataframes into a single dataframe
final_data <- bind_rows(data_list)

# View the first few rows of the stacked dataframe
head(final_data)

ls(final_data)


# List of variables to retain
variables_to_keep <- c(
  "country", "date", "activism", "arrest", "censor", "cooperate", "corruption","coup", 
  "defamationcase", "disaster", "electionactivity", "electionirregularities", 
  "legalaction", "legalchange", "martiallaw", "mobilizesecurity", "protest", 
  "purge", "raid", "threaten", "violencelethal", "violencenonlethal"
)

# Subset the dataset to keep only the specified variables
final_data <- final_data %>%
  select(all_of(variables_to_keep))

# View the first few rows of the filtered dataframe
head(final_data)

# Define the regions and corresponding countries
europe_central_asia <- c("Albania", "Belarus", "Hungary", "Kosovo", "Macedonia", "Moldova", 
                         "Serbia", "Ukraine", "Turkey", "Armenia", "Azerbaijan", "Georgia", 
                         "Kazakhstan", "Kyrgyzstan", "Uzbekistan")
middle_east_north_africa <- c("Algeria", "Mauritania", "Morocco", "Tunisia")
sub_saharan_africa <- c("Angola", "Benin", "Burkina Faso", "Cameroon", "DR Congo", "Ethiopia", 
                        "Ghana", "Kenya", "Liberia", "Malawi", "Mali", "Mozambique", 
                        "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "South Africa", 
                        "South Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe")
east_asia_pacific <- c("Cambodia", "Indonesia", "Malaysia", "Philippines", "Timor Leste", "Solomon Islands")
south_asia <- c("Bangladesh", "India", "Nepal", "Pakistan", "Sri Lanka")
latin_america_caribbean <- c("Colombia", "Dominican Republic", "Ecuador", "El Salvador", 
                             "Guatemala", "Honduras", "Jamaica", "Nicaragua", "Paraguay", "Peru")

# Create the 'region' variable
final_data <- final_data %>%
  mutate(
    region = case_when(
      country %in% europe_central_asia ~ "Europe and Central Asia",
      country %in% middle_east_north_africa ~ "Middle East and North Africa",
      country %in% sub_saharan_africa ~ "Sub-Saharan Africa",
      country %in% east_asia_pacific ~ "East Asia and Pacific",
      country %in% south_asia ~ "South Asia",
      country %in% latin_america_caribbean ~ "Latin America and Caribbean",
      TRUE ~ "Other" # Default value if the country doesn't match any group
    )
  )

# View the first few rows of the updated dataframe
head(final_data)

###############################
## Grid of Shocks Per Region ##
###############################

# Ensure 'date' column is in Date format
final_data$date <- as.Date(final_data$date)

# Get the unique regions
regions <- unique(final_data$region)
regions

region_name <- "Sub-Saharan Africa"
test <- final_data %>% filter(region == region_name)

# Define the x-axis range
min_date <- as.Date("2019-01-01")
max_date <- as.Date("2020-12-01")

# Create and save a plot for each region
for (region_name in regions) {
  print(region_name)
  # Filter data for the current region
  region_data <- final_data %>% filter(region == region_name)
  print(count(region_data, as.factor(region_data$country)))
  
  # Create the grid of plots for the current region
  p <- ggplot(region_data, aes(x = date, y = martiallaw)) +
    geom_line() +  # Line plot
    geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") + # Vertical line for 2020-03
    facet_wrap(~country, scales = "free_x") + # Create one plot per country
    labs(title = paste("Martial Law Shocks Over Time -", region_name),
         x = "Date",
         y = "Number of Martial Law Shocks") +
    theme_minimal() +
    # Set y-axis to show only integer ticks
    scale_y_continuous(breaks = scales::breaks_pretty(n = 1)) + 
    # Set x-axis to show ticks every 3 months and limit to January 2019 to Dec 2020
    scale_x_date(
      limits = c(min_date, max_date),
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    # Customize text appearance for x-axis labels
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  
  # Define the file name for the PDF
  png_file <- paste0(output_path, "/Martial_Law_Shocks_", gsub(" ", "_", region_name), ".png")
  
  # Save the plot as a PDF
  ggsave(filename = png_file, plot = p, device = "png", width = 12, height = 10, dpi = 300)
}

############################## 
## Plot with normalized counts

# Importing data
df_counts <- read.csv("/Users/diegoromero/Dropbox/ML for Peace/Side Projects/Plots for MLP/full-data.csv", stringsAsFactors = FALSE)

# List of variables to retain
variables_to_keep <- c(
  "country", "date", "activism", "arrest", "censor", "cooperate", "corruption","coup", 
  "defamationcase", "disaster", "electionactivity", "electionirregularities", 
  "legalaction", "legalchange", "martiallaw", "mobilizesecurity", "protest", 
  "purge", "raid", "threaten", "violencelethal", "violencenonlethal"
)

# Subset the dataset to keep only the specified variables
df_counts <- df_counts %>%
  select(all_of(variables_to_keep))

## Normalized Counts
# List of variables to normalize
vars_to_normalize <- c(
  "activism", "arrest", "censor", "cooperate", "corruption", "coup",
  "defamationcase", "disaster", "electionactivity", "electionirregularities", 
  "legalaction", "legalchange", "martiallaw", "mobilizesecurity", "protest", 
  "purge", "raid", "threaten", "violencelethal", "violencenonlethal"
)

# Calculate the row sum of the specified variables
df_counts <- df_counts %>%
  rowwise() %>%
  mutate(row_sum = sum(c_across(all_of(vars_to_normalize)), na.rm = TRUE)) %>%
  ungroup()

# Create normalized versions of each variable
df_counts <- df_counts %>%
  mutate(across(all_of(vars_to_normalize), 
                ~ . / row_sum, 
                .names = "n_{col}"))

# Drop the temporary row_sum column if no longer needed
df_counts <- df_counts %>%
  select(-row_sum)

# View the result
head(df_counts)

# Define the regions and corresponding countries
europe_central_asia <- c("Albania", "Belarus", "Hungary", "Kosovo", "Macedonia", "Moldova", 
                         "Serbia", "Ukraine", "Turkey", "Armenia", "Azerbaijan", "Georgia", 
                         "Kazakhstan", "Kyrgyzstan", "Uzbekistan")
middle_east_north_africa <- c("Algeria", "Mauritania", "Morocco", "Tunisia")
sub_saharan_africa <- c("Angola", "Benin", "Burkina Faso", "Cameroon", "DR Congo", "Ethiopia", 
                        "Ghana", "Kenya", "Liberia", "Malawi", "Mali", "Mozambique", 
                        "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "South Africa", 
                        "South Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe")
east_asia_pacific <- c("Cambodia", "Indonesia", "Malaysia", "Philippines", "Timor Leste", "Solomon Islands")
south_asia <- c("Bangladesh", "India", "Nepal", "Pakistan", "Sri Lanka")
latin_america_caribbean <- c("Colombia", "Dominican Republic", "Ecuador", "El Salvador", 
                             "Guatemala", "Honduras", "Jamaica", "Nicaragua", "Paraguay", "Peru")

# Create the 'region' variable
df_counts <- df_counts %>%
  mutate(
    region = case_when(
      country %in% europe_central_asia ~ "Europe and Central Asia",
      country %in% middle_east_north_africa ~ "Middle East and North Africa",
      country %in% sub_saharan_africa ~ "Sub-Saharan Africa",
      country %in% east_asia_pacific ~ "East Asia and Pacific",
      country %in% south_asia ~ "South Asia",
      country %in% latin_america_caribbean ~ "Latin America and Caribbean",
      TRUE ~ "Other" # Default value if the country doesn't match any group
    )
  )

  
##########################################
## Grid of normalized counts Per Region ##
##########################################

# Ensure 'date' column is in Date format
df_counts$date <- as.Date(df_counts$date)

# Get the unique regions
regions <- unique(df_counts$region)
regions

# Define the x-axis range
min_date <- as.Date("2019-01-01")
max_date <- as.Date("2020-12-01")

# Create and save a plot for each region
for (region_name in regions) {
  print(region_name)
  # Filter data for the current region
  region_data <- df_counts %>% filter(region == region_name)
  print(count(region_data, as.factor(region_data$country)))
  
  # Create the grid of plots for the current region
  p <- ggplot(region_data, aes(x = date, y = martiallaw)) +
    geom_line() +  # Line plot
    geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") + # Vertical line for 2020-03
    facet_wrap(~country, scales = "free_x") + # Create one plot per country
    labs(title = paste("Number of Articles about Martial Law Over Time -", region_name),
         x = "Date",
         y = "Number of Articles about Martial Law") +
    theme_minimal() +
    # Set x-axis to show ticks every 3 months and limit to January 2019 to Dec 2020
    scale_x_date(
      limits = c(min_date, max_date),
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    # Customize text appearance for x-axis labels
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
    # Ensure Y-axis shows percentage values
    #scale_y_continuous(labels = scales::label_percent(scale = 1))
  
  # Define the file name for the PDF
  png_file <- paste0(output_path, "/Martial_Law_Trends_", gsub(" ", "_", region_name), ".png")
  
  # Save the plot as a PDF
  ggsave(filename = png_file, plot = p, device = "png", width = 12, height = 10, dpi = 300)
}



##########################################
## Grid of normalized counts Per Region ##
##########################################

# Ensure 'date' column is in Date format
df_counts$date <- as.Date(df_counts$date)

# Get the unique regions
regions <- unique(df_counts$region)
regions

# Define the x-axis range
min_date <- as.Date("2019-01-01")
max_date <- as.Date("2020-12-01")

# Create and save a plot for each region
for (region_name in regions) {
  print(region_name)
  # Filter data for the current region
  region_data <- df_counts %>% filter(region == region_name)
  print(count(region_data, as.factor(region_data$country)))
  
  # Create the grid of plots for the current region
  p <- ggplot(region_data, aes(x = date, y = n_martiallaw * 100)) +
    geom_line() +  # Line plot
    geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") + # Vertical line for 2020-03
    facet_wrap(~country, scales = "free_x") + # Create one plot per country
    labs(title = paste("Share of Articles about Martial Law Over Time -", region_name),
         x = "Date",
         y = "% of Articles about Martial Law") +
    theme_minimal() +
    # Set x-axis to show ticks every 3 months and limit to January 2019 to Dec 2020
    scale_x_date(
      limits = c(min_date, max_date),
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    # Customize text appearance for x-axis labels
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1))
  
  # Define the file name for the PDF
  png_file <- paste0(output_path, "/Martial_Law_NTrends_", gsub(" ", "_", region_name), ".png")
  
  # Save the plot as a PDF
  ggsave(filename = png_file, plot = p, device = "png", width = 12, height = 10, dpi = 300)
}



####################################################
## Grid of normalized counts Per Region: activism ##
####################################################

# Ensure 'date' column is in Date format
df_counts$date <- as.Date(df_counts$date)

# Get the unique regions
regions <- unique(df_counts$region)
regions

# Define the x-axis range
min_date <- as.Date("2019-01-01")
max_date <- as.Date("2020-12-01")

# Create and save a plot for each region
for (region_name in regions) {
  print(region_name)
  # Filter data for the current region
  region_data <- df_counts %>% filter(region == region_name)
  print(count(region_data, as.factor(region_data$country)))
  
  # Create the grid of plots for the current region
  p <- ggplot(region_data, aes(x = date, y = activism)) +
    geom_line() +  # Line plot
    geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") + # Vertical line for 2020-03
    facet_wrap(~country, scales = "free_x") + # Create one plot per country
    labs(title = paste("Number of Articles about Activism Over Time -", region_name),
         x = "Date",
         y = "Number of Articles about Activism") +
    theme_minimal() +
    # Set x-axis to show ticks every 3 months and limit to January 2019 to Dec 2020
    scale_x_date(
      limits = c(min_date, max_date),
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    # Customize text appearance for x-axis labels
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  # Ensure Y-axis shows percentage values
  #scale_y_continuous(labels = scales::label_percent(scale = 1))
  
  # Define the file name for the PDF
  png_file <- paste0(output_path, "/Activism_Trends_", gsub(" ", "_", region_name), ".png")
  
  # Save the plot as a PDF
  ggsave(filename = png_file, plot = p, device = "png", width = 12, height = 10, dpi = 300)
}


####################################################
## Grid of normalized counts & shocks Per Region  ##
####################################################

# Ensure 'date' column is in Date format
df_counts$date <- as.Date(df_counts$date)
final_data$date <- as.Date(final_data$date)

# Merge df_counts and final_data to include the `martiallaw` variable
df_merged <- df_counts %>%
  left_join(final_data %>% select(country, date, martiallaw), by = c("country", "date"))

# Get the unique regions
regions <- unique(df_merged$region)

# Define the x-axis range
min_date <- as.Date("2019-01-01")
max_date <- as.Date("2020-12-01")

# Create and save a plot for each region
for (region_name in regions) {
  print(region_name)
  
  # Filter data for the current region
  region_data <- df_merged %>% filter(region == region_name)
  print(count(region_data, as.factor(region_data$country)))
  
  # Create the grid of plots for the current region
  p <- ggplot(region_data, aes(x = date, y = n_martiallaw * 100)) +
    geom_line() +  # Line plot
    geom_point(data = region_data %>% filter(martiallaw.y == 1),
               aes(x = date, y = n_martiallaw * 100),
               color = "red", size = 2) + # Red dots where martiallaw is 1
    geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") + # Vertical line for 2020-03
    facet_wrap(~country, scales = "free_x") + # Create one plot per country
    labs(title = paste("Share of Articles about Martial Law Over Time -", region_name),
         x = "Date",
         y = "% of Articles about Martial Law") +
    theme_minimal() +
    # Set x-axis to show ticks every 3 months and limit to January 2019 to Dec 2020
    scale_x_date(
      limits = c(min_date, max_date),
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    # Customize text appearance for x-axis labels
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
    # Ensure Y-axis shows percentage values
    scale_y_continuous(labels = scales::label_percent(scale = 1))
  
  # Define the file name for the PNG
  png_file <- paste0(output_path, "/Martial_Law_ShareShock_", gsub(" ", "_", region_name), ".png")
  
  # Save the plot as a PNG
  ggsave(filename = png_file, plot = p, device = "png", width = 12, height = 10, dpi = 300)
}

####################################################
## Coup Plots                                     ##
####################################################
# Merge df_counts and final_data to include the `martiallaw` variable
df_merged <- df_counts %>%
  left_join(final_data %>% select(country, date, coup), by = c("country", "date"))


####################################################
## Normalized counts & shocks                     ##
## Coups in Burkina Faso: Sept 2015, Oct 2016,    ##
## Jan 2022, Sept 2022, Sept 2023       ##
####################################################

# Define the x-axis range
min_date <- as.Date("2021-07-01")
max_date <- as.Date("2024-03-31")

# Define the vertical lines and their colors: blue for attempts, red for coups
vertical_lines <- data.frame(
  dates = as.Date(c("2022-01-01", "2022-09-01","2023-09-01")),
  labels = c("Coup", "Coup", "Attempt") # Labels for the legend
)

# Filter data for the country
country_data <- df_merged %>% filter(country == "Burkina Faso")

# Create the plot for the current country
p <- ggplot(country_data, aes(x = date, y = n_coup * 100)) +
  geom_line() +  # Line plot
  geom_point(data = country_data %>% filter(coup.y == 1),
             aes(x = date, y = n_coup * 100),
             color = "red", size = 2) + # Red dots where coup is 1
  geom_vline(data = vertical_lines, 
             aes(xintercept = dates, color = labels), 
             linetype = "dashed", size = 0.8) + # Vertical lines with legend
  labs(title = "Share of Articles about Coups in Burkina Faso",
       x = "Date",
       y = "% of Articles",
       color = "Event") + # Legend title
  theme_minimal() +
  # Set x-axis to show ticks every 2 months and limit
  scale_x_date(
    limits = c(min_date, max_date),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  # Customize text appearance for x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  # Manually set colors for the vertical lines
  scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red"))

# Print the plot
p

# Define the file name for the PNG
png_file <- paste0(output_path, "/Coup_Burkina-Faso.png")

# Save the plot as a PNG
ggsave(filename = png_file, plot = p, device = "png", width = 10, height = 8, dpi = 300)


####################################################
## Normalized counts & shocks                     ##
## Coups in DR Congo: May 2024                   ##
####################################################

# Define the x-axis range
min_date <- as.Date("2023-11-01")
max_date <- as.Date("2024-11-30")


# Define the vertical lines and their colors: blue for attempts, red for coups
vertical_lines <- data.frame(
  dates = as.Date(c("2024-05-01")),
  labels = c("Attempt") # Labels for the legend
)

# Filter data for the country
country_data <- df_merged %>% filter(country == "DR Congo")

# Create the plot for the current country
p <- ggplot(country_data, aes(x = date, y = n_coup * 100)) +
  geom_line() +  # Line plot
  geom_point(data = country_data %>% filter(coup.y == 1),
             aes(x = date, y = n_coup * 100),
             color = "red", size = 2) + # Red dots where coup is 1
  geom_vline(data = vertical_lines, 
             aes(xintercept = dates, color = labels), 
             linetype = "dashed", size = 0.8) + # Vertical lines with legend
  labs(title = "Share of Articles about Coups in DR Congo",
       x = "Date",
       y = "% of Articles",
       color = "Event") + # Legend title
  theme_minimal() +
  # Set x-axis to show ticks every 2 months and limit
  scale_x_date(
    limits = c(min_date, max_date),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  # Customize text appearance for x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  # Manually set colors for the vertical lines
  scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red"))

# Print the plot
p

# Define the file name for the PNG
png_file <- paste0(output_path, "/Coup_DR-Congo.png")

# Save the plot as a PNG
ggsave(filename = png_file, plot = p, device = "png", width = 10, height = 8, dpi = 300)


####################################################
## Normalized counts & shocks                     ##
## Coups in Ethiopia: Jun 2019                    ##
####################################################

# Define the x-axis range
min_date <- as.Date("2018-12-01")
max_date <- as.Date("2019-12-31")

# Define the vertical lines and their colors: blue for attempts, red for coups
vertical_lines <- data.frame(
  dates = as.Date(c("2019-06-01")),
  labels = c("Attempt") # Labels for the legend
)

# Filter data for the country
country_data <- df_merged %>% filter(country == "Ethiopia")

# Create the plot for the current country
p <- ggplot(country_data, aes(x = date, y = n_coup * 100)) +
  geom_line() +  # Line plot
  geom_point(data = country_data %>% filter(coup.y == 1),
             aes(x = date, y = n_coup * 100),
             color = "red", size = 2) + # Red dots where coup is 1
  geom_vline(data = vertical_lines, 
             aes(xintercept = dates, color = labels), 
             linetype = "dashed", size = 0.8) + # Vertical lines with legend
  labs(title = "Share of Articles about Coups in Ethiopia",
       x = "Date",
       y = "% of Articles",
       color = "Event") + # Legend title
  theme_minimal() +
  # Set x-axis to show ticks every 2 months and limit
  scale_x_date(
    limits = c(min_date, max_date),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  # Customize text appearance for x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  # Manually set colors for the vertical lines
  scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red"))

# Print the plot
p

# Define the file name for the PNG
png_file <- paste0(output_path, "/Coup_Ethiopia.png")

# Save the plot as a PNG
ggsave(filename = png_file, plot = p, device = "png", width = 10, height = 8, dpi = 300)


####################################################
## Normalized counts & shocks                     ##
## Coups in Mali: Aug 2020, May 2021, May 2022    ##
####################################################

# Define the x-axis range
min_date <- as.Date("2020-02-01")
max_date <- as.Date("2022-11-30")

# Define the vertical lines and their colors: blue for attempts, red for coups
vertical_lines <- data.frame(
  dates = as.Date(c("2020-08-01", "2021-05-01","2022-05-01")),
  labels = c("Coup", "Coup", "Attempt") # Labels for the legend
)

# Filter data for the country
country_data <- df_merged %>% filter(country == "Mali")

# Create the plot for the current country
p <- ggplot(country_data, aes(x = date, y = n_coup * 100)) +
  geom_line() +  # Line plot
  geom_point(data = country_data %>% filter(coup.y == 1),
             aes(x = date, y = n_coup * 100),
             color = "red", size = 2) + # Red dots where coup is 1
  geom_vline(data = vertical_lines, 
             aes(xintercept = dates, color = labels), 
             linetype = "dashed", size = 0.8) + # Vertical lines with legend
  labs(title = "Share of Articles about Coups in Mali",
       x = "Date",
       y = "% of Articles",
       color = "Event") + # Legend title
  theme_minimal() +
  # Set x-axis to show ticks every 2 months and limit
  scale_x_date(
    limits = c(min_date, max_date),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  # Customize text appearance for x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  # Manually set colors for the vertical lines
  scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red"))

# Print the plot
p

# Define the file name for the PNG
png_file <- paste0(output_path, "/Coup_Mali.png")

# Save the plot as a PNG
ggsave(filename = png_file, plot = p, device = "png", width = 10, height = 8, dpi = 300)


####################################################
## Normalized counts & shocks                     ##
## Coups in Niger: Mar 2021, Jul 2023             ##
####################################################

# Define the x-axis range
min_date <- as.Date("2020-09-01")
max_date <- as.Date("2024-01-31")

# Define the vertical lines and their colors: blue for attempts, red for coups
vertical_lines <- data.frame(
  dates = as.Date(c("2021-03-01", "2023-07-01")),
  labels = c("Attempt", "Coup") # Labels for the legend
)

# Filter data for the country
country_data <- df_merged %>% filter(country == "Niger")

# Create the plot for the current country
p <- ggplot(country_data, aes(x = date, y = n_coup * 100)) +
  geom_line() +  # Line plot
  geom_point(data = country_data %>% filter(coup.y == 1),
             aes(x = date, y = n_coup * 100),
             color = "red", size = 2) + # Red dots where coup is 1
  geom_vline(data = vertical_lines, 
             aes(xintercept = dates, color = labels), 
             linetype = "dashed", size = 0.8) + # Vertical lines with legend
  labs(title = "Share of Articles about Coups in Niger",
       x = "Date",
       y = "% of Articles",
       color = "Event") + # Legend title
  theme_minimal() +
  # Set x-axis to show ticks every 2 months and limit
  scale_x_date(
    limits = c(min_date, max_date),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  # Customize text appearance for x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  # Manually set colors for the vertical lines
  scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red"))

# Print the plot
p


# Define the file name for the PNG
png_file <- paste0(output_path, "/Coup_Niger.png")

# Save the plot as a PNG
ggsave(filename = png_file, plot = p, device = "png", width = 10, height = 8, dpi = 300)


####################################################
## Normalized counts & shocks                     ##
## Coups in Peru: Dec 2022                        ##
####################################################

# Define the x-axis range
min_date <- as.Date("2022-06-01")
max_date <- as.Date("2023-06-30")

# Define the vertical lines and their colors: blue for attempts, red for coups
vertical_lines <- data.frame(
  dates = as.Date(c("2022-12-01")),
  labels = c("Self-Coup") # Labels for the legend
)

# Filter data for the country
country_data <- df_merged %>% filter(country == "Peru")

# Create the plot for the current country
p <- ggplot(country_data, aes(x = date, y = n_coup * 100)) +
  geom_line() +  # Line plot
  geom_point(data = country_data %>% filter(coup.y == 1),
             aes(x = date, y = n_coup * 100),
             color = "red", size = 2) + # Red dots where coup is 1
  geom_vline(data = vertical_lines, 
             aes(xintercept = dates, color = labels), 
             linetype = "dashed", size = 0.8) + # Vertical lines with legend
  labs(title = "Share of Articles about Coups in Peru",
       x = "Date",
       y = "% of Articles",
       color = "Event") + # Legend title
  theme_minimal() +
  # Set x-axis to show ticks every 2 months and limit
  scale_x_date(
    limits = c(min_date, max_date),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  # Customize text appearance for x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  # Manually set colors for the vertical lines
  scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red", "Self-Coup" = "orange"))

# Print the plot
p


# Define the file name for the PNG
png_file <- paste0(output_path, "/Coup_Peru.png")

# Save the plot as a PNG
ggsave(filename = png_file, plot = p, device = "png", width = 10, height = 8, dpi = 300)


####################################################
## Normalized counts & shocks                     ##
## Coups in Tunisia: Jul 2021                     ##
####################################################

# Define the x-axis range
min_date <- as.Date("2021-01-01")
max_date <- as.Date("2022-01-31")

# Define the vertical lines and their colors: blue for attempts, red for coups
vertical_lines <- data.frame(
  dates = as.Date(c("2021-07-01")),
  labels = c("Self-Coup") # Labels for the legend
)

# Filter data for the country
country_data <- df_merged %>% filter(country == "Tunisia")

# Create the plot for the current country
p <- ggplot(country_data, aes(x = date, y = n_coup * 100)) +
  geom_line() +  # Line plot
  geom_point(data = country_data %>% filter(coup.y == 1),
             aes(x = date, y = n_coup * 100),
             color = "red", size = 2) + # Red dots where coup is 1
  geom_vline(data = vertical_lines, 
             aes(xintercept = dates, color = labels), 
             linetype = "dashed", size = 0.8) + # Vertical lines with legend
  labs(title = "Share of Articles about Coups in Tunisia",
       x = "Date",
       y = "% of Articles",
       color = "Event") + # Legend title
  theme_minimal() +
  # Set x-axis to show ticks every 2 months and limit
  scale_x_date(
    limits = c(min_date, max_date),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  # Customize text appearance for x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  # Manually set colors for the vertical lines
  scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red", "Self-Coup" = "orange"))

# Print the plot
p

# Define the file name for the PNG
png_file <- paste0(output_path, "/Coup_Tunisia.png")

# Save the plot as a PNG
ggsave(filename = png_file, plot = p, device = "png", width = 10, height = 8, dpi = 300)



####################################################
## Normalized counts & shocks                     ##
## Coups in Turkey: Jul 2016                     ##
####################################################

# Define the x-axis range
min_date <- as.Date("2016-01-01")
max_date <- as.Date("2017-01-31")

# Define the vertical lines and their colors: blue for attempts, red for coups
vertical_lines <- data.frame(
  dates = as.Date(c("2016-07-01")),
  labels = c("Attempt") # Labels for the legend
)

# Filter data for the country
country_data <- df_merged %>% filter(country == "Turkey")

# Create the plot for the current country
p <- ggplot(country_data, aes(x = date, y = n_coup * 100)) +
  geom_line() +  # Line plot
  geom_point(data = country_data %>% filter(coup.y == 1),
             aes(x = date, y = n_coup * 100),
             color = "red", size = 2) + # Red dots where coup is 1
  geom_vline(data = vertical_lines, 
             aes(xintercept = dates, color = labels), 
             linetype = "dashed", size = 0.8) + # Vertical lines with legend
  labs(title = "Share of Articles about Coups in Turkey",
       x = "Date",
       y = "% of Articles",
       color = "Event") + # Legend title
  theme_minimal() +
  # Set x-axis to show ticks every 2 months and limit
  scale_x_date(
    limits = c(min_date, max_date),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  # Customize text appearance for x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  # Manually set colors for the vertical lines
  scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red", "Self-Coup" = "orange"))

# Print the plot
p

# Define the file name for the PNG
png_file <- paste0(output_path, "/Coup_Turkey.png")

# Save the plot as a PNG
ggsave(filename = png_file, plot = p, device = "png", width = 10, height = 8, dpi = 300)



####################################################
## Normalized counts & shocks                     ##
## Coups in Zimbabwe: Nov 2019                     ##
####################################################

# Define the x-axis range
min_date <- as.Date("2019-05-01")
max_date <- as.Date("2020-05-31")

# Define the vertical lines and their colors: blue for attempts, red for coups
vertical_lines <- data.frame(
  dates = as.Date(c("2019-11-01")),
  labels = c("Attempt") # Labels for the legend
)

# Filter data for the country
country_data <- df_merged %>% filter(country == "Zimbabwe")

# Create the plot for the current country
p <- ggplot(country_data, aes(x = date, y = n_coup * 100)) +
  geom_line() +  # Line plot
  geom_point(data = country_data %>% filter(coup.y == 1),
             aes(x = date, y = n_coup * 100),
             color = "red", size = 2) + # Red dots where coup is 1
  geom_vline(data = vertical_lines, 
             aes(xintercept = dates, color = labels), 
             linetype = "dashed", size = 0.8) + # Vertical lines with legend
  labs(title = "Share of Articles about Coups in Zimbabwe",
       x = "Date",
       y = "% of Articles",
       color = "Event") + # Legend title
  theme_minimal() +
  # Set x-axis to show ticks every 2 months and limit
  scale_x_date(
    limits = c(min_date, max_date),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  # Customize text appearance for x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  # Ensure Y-axis shows percentage values
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  # Manually set colors for the vertical lines
  scale_color_manual(values = c("Attempt" = "blue", "Coup" = "red", "Self-Coup" = "orange"))

# Print the plot
p

# Define the file name for the PNG
png_file <- paste0(output_path, "/Coup_Zimbabwe.png")

# Save the plot as a PNG
ggsave(filename = png_file, plot = p, device = "png", width = 10, height = 8, dpi = 300)





