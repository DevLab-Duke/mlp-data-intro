# Descriptive Maps Analysis
# Refactored from Maps_MVC.Rmd
# Creates temporal maps showing civic space coverage patterns
# 
# Author: Refactored version
# Date: 2025-01-10

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(maps)
library(here)
library(readr)

# Load constants and variable mappings
source(here("build_data", "constants.R"))

# Set output directory
output_path <- here("writing", "descriptive_maps", "figures")
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# Load data from repository
data <- read_csv(here("data", "final-counts", "full-civic-data.csv"))

# Read civic variable mapping from cs_vars.csv
cs_vars <- read_csv(here("data", "cs_vars.csv"))

# Create mapping from variable names to labels
var_labels <- setNames(cs_vars$name, cs_vars$id)

message(paste("Loaded data for", length(unique(data$country)), "countries"))
message(paste("Date range:", min(data$date), "to", max(data$date)))

# -------------------------------------------------------------------------
# MAP 1: CIVIC SPACE PERCENTAGE BY COUNTRY AND YEAR

# Prepare data for percentage maps
vars_to_keep <- c("date", "country", civic, "total_articles")

df_map1 <- data %>% 
  select(all_of(vars_to_keep)) %>% 
  mutate(
    total_cs = rowSums(across(all_of(civic), ~ .)),
    country = ifelse(country == "DR Congo", "Democratic Republic of the Congo", country),
    year = format(as.Date(date, "%Y-%m-%d"), "%Y")
  ) %>% 
  group_by(year, country) %>% 
  summarise(
    tot_yr_cs = sum(total_cs),
    tot_yr_art = sum(total_articles),
    p_cs = tot_yr_cs/tot_yr_art,
    .groups = 'drop'
  )

# Setting map parameters
map_theme <- theme(
  axis.title.x = element_blank(), axis.text.x = element_blank(), 
  axis.ticks.x = element_blank(), 
  axis.title.y = element_blank(), axis.text.y = element_blank(), 
  axis.ticks.y = element_blank(),
  legend.key = element_rect(),
  legend.key.height = unit(0.5, 'cm'),
  legend.key.width = unit(0.5, 'cm'),
  legend.text = element_text(size = 5), 
  legend.title = element_text(size = 6),
  legend.box.background = element_rect(colour = "black")
)

# Load world map data
map <- map_data('world')
map <- map %>% 
  filter(region != "Antarctica") %>%  
  rename(country = region)

# Set map limits to mimic the ones for the RAI dataset
x_lim = c(-90, 142)
y_lim = c(-33, 52)

# Creating an expanded dataset for countries not in our data
miss_map <- left_join(map, df_map1, by = "country")
miss_cty_expanded <- miss_map %>% 
  filter(is.na(year)) %>% 
  distinct(country) %>%
  tidyr::crossing(year = unique(df_map1$year), tot_yr_cs = NA, tot_yr_art = NA, p_cs = NA)

# Handle Namibia special case (no data before 2017)
new_obs <- data.frame(
  country = "Namibia",
  year = as.character(2012:2016),
  tot_yr_cs = NA,
  tot_yr_art = NA,
  p_cs = NA
)

# Add Namibia to the missing data
miss_cty_expanded <- bind_rows(miss_cty_expanded, new_obs)

# Combining the final map data
complete_data <- bind_rows(df_map1, miss_cty_expanded)
map1 <- left_join(map, complete_data, by = "country")

# Plot the map for the appendix (all years) with legend in blank space
appendix_plot <- ggplot(map1, aes(x = long, y = lat, group = group, fill = p_cs)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "#F1E980", high = "#753202", na.value = "lightgray") +
  labs(title = "Civic Space Articles per Country (yearly averages)",
       fill = "Civic Space \nArticles (%)") +
  coord_map(xlim = x_lim, ylim = y_lim) +
  theme_bw() + 
  facet_wrap(~ year) +
  map_theme +
  # Place legend in the bottom-right area where blank panels would be
  theme(legend.position = c(0.95, 0.08),
        legend.justification = c(1, 0),
        legend.key.height = unit(0.8, 'cm'),
        legend.key.width = unit(0.8, 'cm'),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10),
        legend.direction = "horizontal" )

ggsave(file.path(output_path, "civic_space_percentage_appendix.png"), 
       plot = appendix_plot, width = 12, height = 6, units = "in")

# Plot the map for the paper (selected years)
paper_plot <- ggplot(map1 %>% filter(year %in% c(2012, 2016, 2020, 2024)), 
                     aes(x = long, y = lat, group = group, fill = p_cs)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "#F1E980", high = "#753202", na.value = "lightgray") +
  labs(title = "Civic Space Articles per Country (yearly averages)",
       fill = "Civic Space \nArticles (%)") +
  coord_map(xlim = x_lim, ylim = y_lim) +
  theme_bw() + 
  facet_wrap(~ year) +
  map_theme

ggsave(file.path(output_path, "civic_space_percentage_paper.png"), 
       plot = paper_plot, width = 12, height = 6, units = "in")

# -------------------------------------------------------------------------
# MAP 2: MOST COMMON CIVIC EVENT BY COUNTRY AND YEAR

# Prepare data for dominant event maps
df_map2 <- data %>% 
  select(all_of(vars_to_keep)) %>% 
  mutate(
    country = ifelse(country == "DR Congo", "Democratic Republic of the Congo", country),
    year = format(as.Date(date, "%Y-%m-%d"), "%Y")
  ) %>% 
  group_by(country, year) %>% 
  summarise(across(all_of(civic), sum, na.rm = TRUE), .groups = 'drop')

# Find the most common event for each country-year
df_map2$max_var <- colnames(df_map2[, civic])[apply(df_map2[, civic], 1, which.max)]

# Create clean dataset with labels
df_map2_clean <- df_map2 %>% 
  select(year, country, max_var) %>%
  mutate(max_var_label = var_labels[max_var])

# Creating an expanded dataset for countries not in our data
miss_map2 <- left_join(map, df_map2_clean, by = "country")
miss_cty_expanded2 <- miss_map2 %>% 
  filter(is.na(max_var)) %>% 
  distinct(country) %>%
  tidyr::crossing(year = unique(df_map2_clean$year), max_var = NA, max_var_label = NA)

# Handle Namibia special case (no data before 2017)
new_obs2 <- data.frame(
  country = "Namibia",
  year = as.character(2012:2016),
  max_var = NA,
  max_var_label = NA
)

# Add Namibia to the missing data
miss_cty_expanded2 <- bind_rows(miss_cty_expanded2, new_obs2)

# Combining the final map data
complete_data2 <- bind_rows(df_map2_clean, miss_cty_expanded2)
map2 <- left_join(map, complete_data2, by = "country")

# Plot the map for the appendix (all years) with legend in blank space
appendix_plot2 <- ggplot(map2, aes(x = long, y = lat, group = group)) +
  # First layer: NA countries in light gray
  geom_polygon(data = map2[is.na(map2$max_var_label), ], fill = "lightgray", color = "black") +
  # Second layer: countries with data
  geom_polygon(data = map2[!is.na(map2$max_var_label), ], aes(fill = max_var_label), color = "black") +
  scale_fill_discrete(na.translate = FALSE) +
  labs(title = "Most Common Civic Space Event per Country",
       fill = "Event \nType") +
  coord_map(xlim = x_lim, ylim = y_lim) +
  theme_bw() + 
  facet_wrap(~ year) +
  map_theme +
  # Place legend in the bottom-right area where blank panels would be
  theme(legend.position = c(0.95, 0.02),
        legend.justification = c(1, 0),
        legend.key.height = unit(0.6, 'cm'),
        legend.key.width = unit(0.6, 'cm'),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 9),
        # Make legend more compact for the many event types
        legend.spacing.y = unit(0.1, 'cm'),
        legend.direction = "horizontal" )

ggsave(file.path(output_path, "dominant_event_appendix.png"), 
       plot = appendix_plot2, width = 12, height = 6, units = "in")

# Plot the map for the paper (selected years)
paper_data <- map2 %>% filter(year %in% c(2012, 2016, 2020, 2024))
paper_plot2 <- ggplot(paper_data, aes(x = long, y = lat, group = group)) +
  # First layer: NA countries in light gray
  geom_polygon(data = paper_data[is.na(paper_data$max_var_label), ], fill = "lightgray", color = "black") +
  # Second layer: countries with data
  geom_polygon(data = paper_data[!is.na(paper_data$max_var_label), ], aes(fill = max_var_label), color = "black") +
  scale_fill_discrete(na.translate = FALSE) +
  labs(title = "Most Common Civic Space Event per Country",
       fill = "Event \nType") +
  coord_map(xlim = x_lim, ylim = y_lim) +
  theme_bw() + 
  facet_wrap(~ year) +
  map_theme

ggsave(file.path(output_path, "dominant_event_paper.png"), 
       plot = paper_plot2, width = 12, height = 6, units = "in")

