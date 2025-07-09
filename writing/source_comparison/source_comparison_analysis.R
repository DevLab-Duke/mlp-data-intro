# Source Comparison Analysis
# Refactored from international_vs_national_graphs.Rmd
# Compares international, regional, and local source coverage patterns
# Date: 2025-01-27

library(ggplot2)
library(stringr)
library(lubridate)
library(scales)
library(ggpubr)
library(here)
library(dplyr)
library(grid)
library(gridExtra)
library(tidyr)
source(here("build_data", "constants.R"))

# Set output directory
output_path <- here("writing", "source_comparison", "figures")

# Load data from repository's data/0-civic-by-source/
data_path <- here("data", "0-civic-by-source")
countries <- list.files(path = data_path, pattern = "\\.csv$")

# Read each file into a list of data frames
file_list <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)
df_list <- lapply(paste0(file_list), read.csv)
counts_combined <- bind_rows(df_list)

message(paste("Loaded data for", length(countries), "countries"))

# Use source classifications from constants.R
# Remove .csv extension from source names for matching
international_sources <- gsub("\\.csv$", "", isources)
regional_sources <- gsub("\\.csv$", "", rsources)

# Create source type flags
counts_combined$international <- ifelse(counts_combined$source %in% international_sources, 1, 0)
counts_combined$regional <- ifelse(counts_combined$source %in% regional_sources, 1, 0)

# Aggregate by country, date, and source type
counts_combined <- counts_combined %>%
  select(!source) %>%
  group_by(country, date, international, regional) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  as.data.frame()

# Calculate percentages across source-types
counts_combined %>% 
  group_by(international, regional) %>% 
  summarise(sum_articles = sum(total_articles), .groups = 'drop') %>%
  mutate(percentage = (sum_articles / sum(sum_articles)) * 100)

# Calculate true article totals by country-month
# We can choose to normalize by either measure, depending on what we're interested in
# For this analysis, we normalize by `total_articles`, which captures total articles per source-type (i.e. domestic, regional, international)
counts_combined$total_articles_country <- 0
for(i in unique(counts_combined$country)) {
  for(j in unique(counts_combined$date)) {
    total_articles <- sum(counts_combined[
      counts_combined$country == i & counts_combined$date == j, 
      "total_articles"
    ])
    counts_combined[
      counts_combined$country == i & counts_combined$date == j,
      "total_articles_country"
    ] <- total_articles
  }
}

# Normalize counts by `total_articles`; exclude metadata columns
numeric_cols <- which(sapply(counts_combined, is.numeric) & 
                           !names(counts_combined) %in% c("international", "regional", "cs_999", "total_articles", "total_articles_country"))


# -------------------------------------------------------------------------
# NORMALIZATION

# Create version of data where counts are raw
# This tells us about how much data we get from different source-types
counts_combined_raw <- counts_combined 

# Create version of data where counts are normalized by the source-type (separate for international, regional, domestic)
# This tells us the counterfactual of what our normalized counts would look like if we only had international+regional
counts_combined_nst <- counts_combined %>%
  mutate(across(all_of(numeric_cols), ~ .x / total_articles))

# Create version of data where counts are normalized by total articles (combining international, regional, and domestic)
# This tells us about where most of the information in our counts is coming from
counts_combined_nct <- counts_combined %>%
  mutate(across(all_of(numeric_cols), ~ .x / total_articles_country))

process_counts_data <- function(data) {
  data %>%
    # Replace NAs with 0 (numerator == 0 if denominator == 0)
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    # Create total civic space variable
    mutate(total_articles_civic = rowSums(select(., all_of(civic)), na.rm = TRUE)) %>%
    # Clean date format
    mutate(date = as.Date(date)) %>%
    # Create non_local variable
    mutate(non_local = if_else(data$international == 1 | data$regional == 1, 1, 0))
  
}

counts_combined_raw = process_counts_data(counts_combined_raw)
counts_combined_nst = process_counts_data(counts_combined_nst)
counts_combined_nct = process_counts_data(counts_combined_nct)


# Store processed data
data <- counts_combined_nst

# -------------------------------------------------------------------------
# GENERATE INDONESIA CASE STUDY PLOTS

# Filter Indonesia data for 2024
indonesia <- counts_combined_nst %>%
  filter(country == "Indonesia") %>%
  select(date, international, regional, corruption, arrest) %>%
  filter(date > "2023-12-01" & date < "2025-01-01")

# Create source type labels
indonesia$source_type <- case_when(
  indonesia$international == 1 ~ "International",
  indonesia$regional == 1 ~ "Regional",
  TRUE ~ "Domestic"
)

# Convert to factor with desired order
indonesia$source_type <- factor(indonesia$source_type, 
                                levels = c("Domestic", "Regional", "International"))

# Reshape data from wide to long format
indonesia_long <- indonesia %>%
  pivot_longer(cols = c(corruption, arrest),
               names_to = "metric",
               values_to = "value") %>%
  mutate(
    metric = case_when(
      metric == "corruption" ~ "Corruption",
      metric == "arrest" ~ "Arrest"
    ),
    value = value
  )

# Color scheme
colors <- c("International" = "red", "Domestic" = "blue", "Regional" = "pink")

# Single plot with facet_wrap
indonesia_plot <- ggplot(indonesia_long, aes(x = date, y = value, 
                                             group = source_type, color = source_type)) +
  geom_line() +
  facet_wrap(~metric, ncol = 3, scales = "free_y") +
  scale_color_manual(name = "Source Type:", values = colors) +
  labs(title = "Share of articles published by source", 
       y = "% of articles published by source type\n (i.e. Corruption Domestic Articles / Total Domestic Articles)", 
       x = "") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    strip.text = element_text(hjust = 0.5),
    legend.position = c(.15, .8)
  )

ggsave(file.path(output_path, "indonesia_int_vs_local.png"), 
       plot = indonesia_plot, height = 5, width = 7)


# -------------------------------------------------------------------------
# GENERATE TRUE VS FALSE SPIKE COMPARISON PLOTS

# Ghana - good spike example
ghana_spike <- counts_combined_raw %>%
  filter(country == "Ghana" & international == 0 & regional == 0) %>%
  filter(date < "2023-10-01")

true_spike_plot <- ggplot(ghana_spike, aes(x = date, y = total_articles_country)) +
  geom_line(color = "blue") +
  labs(x = NULL, y = "Total number of articles", title = "Ghana: True Spike") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Zambia - false spike (from earlier data producted before we fixed the date parsing)
zambia_file <- here("writing", "source_comparison", "zambia_old_data.csv")
  zambia_old <- read.csv(zambia_file) %>%
    mutate(total = as.numeric(total),
           date = as.Date(date))
  
false_spike_plot <- ggplot(zambia_old, aes(x = date, y = total)) +
  geom_line(color = "red") +
  labs(x = NULL, y = "Total number of articles", title = "Zambia: False Spike") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Combine plots
combined_spike_plot <- grid.arrange(
  true_spike_plot, false_spike_plot,
  nrow = 1)

ggsave(file.path(output_path, "true_false_spike.png"), 
       plot = combined_spike_plot, height = 5, width = 7)
  

# -------------------------------------------------------------------------
# GENERATE INTERNATIONAL SHARE ANALYSIS
# "Of all arrest articles, 70% come from domestic sources and 30% from international sources"

# Combine international and regional as non-local
# We're using both nst and nct in this section

# Calculate correlations between international and local coverage by country
countries_unique <- unique(counts_combined_nst$country)
                           
# Create correlations matrix
correlations <- matrix(NA, nrow = length(countries_unique), ncol = length(civic))

# Use nst because we want to compare the normalized counts as-if we only had counts for that source-type
for(i in seq_along(countries_unique)) {
  country_data <- counts_combined_nst[counts_combined_nst$country == countries_unique[i], ]
  for(j in seq_along(civic)) {
    local_data <- country_data[country_data$non_local == 0, civic[j]]
    intl_data <- country_data[country_data$non_local == 1, civic[j]]
    
    # Ensure we have data for both local and international, and same length
    if(length(local_data) > 0 && length(intl_data) > 0 && length(local_data) == length(intl_data)) {
      cor_val <- cor(local_data, intl_data, use = "complete.obs")
      correlations[i, j] <- ifelse(is.na(cor_val), 0, cor_val)
    } else if(length(local_data) > 0 && length(intl_data) > 0) {
      # If different lengths, correlate the time series by matching dates
      local_df <- country_data[country_data$non_local == 0, c("date", civic[j])]
      intl_df <- country_data[country_data$non_local == 1, c("date", civic[j])]
      merged_df <- merge(local_df, intl_df, by = "date", suffixes = c("_local", "_intl"))
      
      if(nrow(merged_df) > 1) {
        cor_val <- cor(merged_df[, paste0(civic[j], "_local")], 
                      merged_df[, paste0(civic[j], "_intl")], 
                      use = "complete.obs")
        correlations[i, j] <- ifelse(is.na(cor_val), 0, cor_val)
      }
    }
  }
}

rm(country_data)

## Be aware of zero-variance country-event pairs; which are currently treated as 0 by previous code
zero_variance_summary <- counts_combined_nst %>%
  group_by(country, non_local) %>%
  summarise(across(all_of(civic), ~ sd(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(civic), names_to = "civic_variable", values_to = "std_dev") %>%
  mutate(local_or_intl = ifelse(non_local == 0, "local", "international")) %>%
  filter(std_dev == 0 | is.na(std_dev)) %>%
  select(country, civic_variable, local_or_intl, std_dev)
# print(zero_variance_summary)

# Calculate average correlations
average_cor <- colMeans(correlations, na.rm = TRUE)

# Aggregate by non-local flag
# use country-level normalization to measure share of articles from each source-type
data_aggregated <- counts_combined_nct %>%
  group_by(non_local) %>%
  summarise(across(c(all_of(civic), total_articles, total_articles_country, total_articles_civic), 
                   sum),
            .groups = "drop")

# Calculate topic shares
topic_share <- data_aggregated
for(i in names(topic_share)) {
  if(is.numeric(topic_share[[i]]) && i != "non_local") {
    topic_share[[i]] <- topic_share[[i]] / sum(topic_share[[i]], na.rm = TRUE)
  }
}

# Read civic variable mapping from cs_vars.csv
cs_vars <- readr::read_csv(here::here("data", "cs_vars.csv"))

# Create renamed columns for plotting
topic_names <- setNames(cs_vars$name, cs_vars$id)

# Prepare data for plotting
plot_data <- data.frame(
  variable = character(0),
  local = numeric(0),
  international = numeric(0),
  average_cor = numeric(0)
)

for(i in seq_along(topic_names)) {
  var_name <- names(topic_names)[i]
  if(var_name %in% names(topic_share)) {
    local_val <- topic_share[topic_share$non_local == 0, ][[var_name]]
    intl_val <- topic_share[topic_share$non_local == 1, ][[var_name]]
    cor_val <- average_cor[match(var_name, civic)]
    
    plot_data <- rbind(plot_data, data.frame(
      variable = topic_names[i],
      local = ifelse(length(local_val) > 0, local_val, 0),
      international = ifelse(length(intl_val) > 0, intl_val, 0),
      average_cor = ifelse(is.na(cor_val), 0, cor_val)
    ))
  }
}

# Create stacked data for plotting
plot_data_long <- plot_data %>%
  pivot_longer(cols = c("local", "international"), 
              names_to = "source_type", values_to = "value") %>%
  mutate(source_type = case_when(
    source_type == "local" ~ "Domestic",
    source_type == "international" ~ "International"
  ))

# Order by correlation
plot_data_long$variable <- factor(plot_data_long$variable, 
                                 levels = unique(plot_data$variable[order(plot_data$average_cor)]))

# Create the plot
colors <- c("Domestic" = "blue", "International" = "chartreuse4")

# Format correlation labels (remove leading zero)
plot_data_long$cor_label <- formatC(plot_data_long$average_cor, digits = 2, format = "f")
plot_data_long$cor_label <- str_replace(plot_data_long$cor_label, "^0\\.", ".")
plot_data_long$cor_label <- str_replace(plot_data_long$cor_label, "^-0\\.", "-.")

int_share_plot <- ggplot(plot_data_long, aes(x = variable)) +
  geom_bar(aes(y = value, fill = source_type), stat = "identity", position = "fill", alpha = 0.5) +
  labs(y = "Share of articles from each source type", x = NULL, title = "Coverage by domestic and international sources is weakly correlated") +
  scale_fill_manual(name = "Source Type:", values = colors) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y = average_cor + 0.15, label = cor_label), vjust = 1, color = "black") +
  geom_point(aes(y = average_cor), color = "darkred")

ggsave(file.path(output_path, "int_share_norm.png"), 
       plot = int_share_plot, height = 5, width = 7)



# -------------------------------------------------------------------------
# GENERATE EVENT SHARE ANALYSIS
# "Domestic sources publish 7 arrest articles per 1,000 articles, while international sources publish 3 arrest articles per 1,000 articles"

# Calculate mean shares by source type
event_share <- counts_combined_nst %>%
  select(all_of(c("non_local", civic))) %>%
  group_by(non_local) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame()


# Prepare plotting data
plot_data <- data.frame(
  variable = character(0),
  local = numeric(0),
  international = numeric(0)
)

for(i in seq_along(topic_names)) {
  var_name <- names(topic_names)[i]
  if(var_name %in% names(event_share)) {
    local_val <- event_share[event_share$non_local == 0, ][[var_name]]
    intl_val <- event_share[event_share$non_local == 1, ][[var_name]]
    
    plot_data <- rbind(plot_data, data.frame(
      variable = topic_names[i],
      local = ifelse(length(local_val) > 0, local_val, 0),
      international = ifelse(length(intl_val) > 0, intl_val, 0)
    ))
  }
}

# Order by international coverage
plot_data$variable <- factor(plot_data$variable, 
                            levels = plot_data$variable[order(plot_data$international)])

colors <- c("Domestic" = "blue", "International" = "chartreuse4")

event_share_plot <- ggplot(plot_data, aes(x = variable)) +
  geom_bar(aes(y = international * 10000, fill = "International"), 
           stat = "identity", alpha = 0.5) +
  geom_bar(aes(y = local, fill = "Domestic"), 
           stat = "identity", alpha = 0.2, position = "dodge") +
  labs(y = "Frequency of articles by source type", x = "", 
       title = "Domestic and international Sources by Volume") +
  scale_fill_manual(name = "Source Type:", values = colors) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 90), #, hjust = 1
        legend.position = c(.2, 0.8),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(output_path, "event_share_total.png"), 
       plot = event_share_plot, height = 5, width = 7)


# -------------------------------------------------------------------------
# GENERATE CORRELATION BY COUNTRY ANALYSIS


# Find total number of non-local articles per country
intl_aggregated <- data %>%
  filter(non_local == 1) %>%
  group_by(country) %>%
  summarise(int_articles = sum(total_articles, na.rm = TRUE), .groups = 'drop') %>%
  mutate(log_int_articles = log(int_articles))

# Calculate country-level correlations and international article counts
country_stats <- data.frame(
  country = countries_unique,
  correlation = rowMeans(correlations, na.rm = TRUE)
)

country_stats = merge(country_stats, intl_aggregated)

# Create scatter plot
country_plot <- ggplot(country_stats, aes(x = log_int_articles, y = correlation)) +
  geom_point(aes(color = country)) +
  labs(y = "Correlation between Domestic and International Coverage", x = "Total Number of International Articles (log)", 
       title = NULL) +
  theme_bw() +
  theme(legend.position = "none") 

# Add labels for specific countries
highlight_countries <- c("Timor Leste", "India", "Turkey", "Kosovo", "Ukraine")
highlight_data <- country_stats[country_stats$country %in% highlight_countries, ]

country_plot <- country_plot + geom_text(data = highlight_data, 
                                         aes(log_int_articles, correlation, label = country),
                                         vjust = -0.5)

ggsave(file.path(output_path, "int_nat_corr_by_country.png"), 
       plot = country_plot, height = 5, width = 7)

