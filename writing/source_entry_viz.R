# Source Entry Visualization Script
# Creates a heatmap showing the count of active sources for each country-month
# Issue #13: Visualize Source Entries

library(tidyverse)
library(ggplot2)
library(here)
library(tidyr)
library(viridis)
library(lubridate)
source(here("build_data", "constants.R"))

# Load the source entries data
source_entries <- readRDS(here("data", "1-source_entries.rds"))

# Display structure of the data
cat("Source entries data structure:\n")
str(source_entries)
cat("\nFirst few rows:\n")
head(source_entries)
cat("\nFirst few columns:\n")
head(source_entries[, 1:6])

# Transform data for heatmap
# The data has columns: date, and then one column for each source
# We need to transform this to have country information

# First, let's understand which sources belong to which countries
# We'll need to map sources to countries using the constants.R functions

# Create a mapping of sources to countries
source_to_country <- data.frame(
  source = character(),
  country = character(),
  stringsAsFactors = FALSE
)

# Load civic data to get list of countries
civic_files <- list.files(here("data", "0-civic-by-source"), pattern = "\\.csv$", full.names = TRUE)
countries <- basename(civic_files) %>% str_remove("\\.csv$")

# For each country, get its local sources and add to mapping
for (country in countries) {
  tryCatch({
    local_sources <- local_source_select(country)$lsources
    if (length(local_sources) > 0) {
      # Remove .csv extension if present
      local_sources <- str_remove(local_sources, "\\.csv$")
      
      # Create mapping entries
      country_mapping <- data.frame(
        source = local_sources,
        country = country,
        stringsAsFactors = FALSE
      )
      source_to_country <- rbind(source_to_country, country_mapping)
    }
  }, error = function(e) {
    cat("Error processing country:", country, "\n")
    cat("Error message:", e$message, "\n")
  })
}

# Transform the source_entries data
# Convert from wide to long format, then aggregate by country
source_entries_long <- source_entries %>%
  pivot_longer(cols = -date, names_to = "source", values_to = "active") %>%
  left_join(source_to_country, by = "source") %>%
  filter(!is.na(country)) %>%  # Remove sources not mapped to any country
  group_by(country, date) %>%
  summarise(
    active_sources = sum(active, na.rm = TRUE),
    .groups = "drop"
  )

# Add region information for each country
country_regions <- list(
  "EE_CA" = c("Albania", "Armenia", "Azerbaijan", "Belarus", "Georgia", "Hungary", 
              "Kazakhstan", "Kosovo", "Kyrgyzstan", "Macedonia", "Moldova", "Serbia", 
              "Turkey", "Ukraine", "Uzbekistan"),
  "MENA" = c("Algeria", "Morocco", "Tunisia"),
  "LAC" = c("Colombia", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", 
            "Guatemala", "Honduras", "Jamaica", "Nicaragua", "Panama", "Paraguay", "Peru"),
  "EA" = c("Bangladesh", "Cambodia", "India", "Indonesia", "Malaysia", "Nepal", 
           "Pakistan", "Philippines", "Solomon Islands", "Sri Lanka", "Timor Leste"),
  "SSA" = c("Angola", "Benin", "Burkina Faso", "Cameroon", "DR Congo", "Ethiopia", 
            "Ghana", "Kenya", "Liberia", "Malawi", "Mali", "Mauritania", "Mozambique", 
            "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "South Africa", 
            "South Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe")
)

# Add region information to the data
source_entries_long <- source_entries_long %>%
  mutate(region = case_when(
    country %in% country_regions$EE_CA ~ "Eastern Europe\n& Central Asia",
    country %in% country_regions$MENA ~ "Middle East\n& North Africa",
    country %in% country_regions$LAC ~ "Latin America\n& Caribbean",
    country %in% country_regions$EA ~ "East Asia",
    country %in% country_regions$SSA ~ "Sub-Saharan\nAfrica",
    TRUE ~ "Other"
  ))

# Format dates for better display
source_entries_long <- source_entries_long %>%
  mutate(
    date = as.Date(date),
    year_month = format(date, "%Y-%m"),
    year = year(date),
    month = month(date)
  )

# Create the heatmap
p <- ggplot(source_entries_long, aes(x = date, y = reorder(country, active_sources, FUN = max), 
                                    fill = active_sources)) +
  geom_tile() +
  facet_grid(region ~ ., scales = "free_y", space = "free_y") +
  scale_fill_viridis_c(name = "Active\nSources", option = "plasma") +
  labs(
    title = "Number of Active Sources by Country-Month",
    x = "Date",
    y = "Country",
    caption = "Source: HQMARC corpus data"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text.y = element_text(angle = 0, size = 9),
    strip.background = element_rect(fill = "grey90"),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Print the plot
print(p)

# Save the plot
ggsave(here("writing", "source_entry_heatmap.png"), plot = p, 
       width = 12, height = 10, dpi = 300)

# Print summary statistics
cat("\nSummary of active sources by country:\n")
summary_stats <- source_entries_long %>%
  group_by(country, region) %>%
  summarise(
    mean_active = mean(active_sources, na.rm = TRUE),
    max_active = max(active_sources, na.rm = TRUE),
    min_active = min(active_sources, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(region, desc(mean_active))

print(summary_stats)

cat("\nVisualization complete! Heatmap saved as 'source_entry_heatmap.png' in the writing folder.\n")