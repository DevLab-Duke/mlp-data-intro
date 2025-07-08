# Source Comparison Analysis
# Refactored from international_vs_national_graphs.Rmd
# Compares international, regional, and local source coverage patterns
# 
# Author: Refactored version
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
                           !names(counts_combined) %in% c("international", "regional", "cs_999", "total_articles_country"))

counts_combined[, numeric_cols] <- counts_combined[, numeric_cols] / counts_combined$total_articles_country


# Replace NAs with 0
counts_combined[is.na(counts_combined)] <- 0

# Create total civic space variable using civic events from constants.R
counts_combined$total_articles_civic <- rowSums(counts_combined[, civic], na.rm = TRUE)

# Clean date format
counts_combined$date <- as.Date(counts_combined$date)

# Store processed data
data <- counts_combined

# === GENERATE TRUE VS FALSE SPIKE COMPARISON PLOTS ===

# Ghana - good spike example
ghana_spike <- data %>%
  filter(country == "Ghana" & international == 0 & regional == 0) %>%
  filter(date < "2023-10-01")

true_spike_plot <- ggplot(ghana_spike, aes(x = date, y = total_articles_country)) +
  geom_line(color = "blue") +
  labs(x = "Date", y = "Article Totals", title = "Ghana: True Spike") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Zambia - false spike (from earlier data producted before we fixed the date parsing)
zambia_file <- here("writing", "source_comparison", "zambia_old_data.csv")
  zambia_old <- read.csv(zambia_file) %>%
    mutate(total = as.numeric(total),
           date = as.Date(date))
  
false_spike_plot <- ggplot(zambia_old, aes(x = date, y = total)) +
  geom_line(color = "red") +
  labs(x = "Date", y = "Article Totals", title = "Zambia: False Spike") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Combine plots
combined_spike_plot <- grid.arrange(
  true_spike_plot, false_spike_plot,
  nrow = 1)

ggsave(file.path(output_path, "true_false_spike.jpg"), 
       plot = combined_spike_plot, height = 5, width = 7)
  

# === GENERATE INDONESIA CASE STUDY PLOTS ===

# Filter Indonesia data for 2024
indonesia <- data %>%
  filter(country == "Indonesia") %>%
  select(date, international, regional, corruption, arrest) %>%
  filter(date > "2023-12-01" & date < "2024-07-01")

# Create source type labels
indonesia$source_type <- case_when(
  indonesia$international == 1 ~ "International",
  indonesia$regional == 1 ~ "Regional",
  TRUE ~ "Domestic"
)

# Reshape data from wide to long format
indonesia_long <- indonesia %>%
  pivot_longer(cols = c(corruption, arrest),
               names_to = "metric",
               values_to = "value") %>%
  mutate(
    metric = case_when(
      metric == "corruption" ~ "Corruption Reporting",
      metric == "arrest" ~ "Arrest Reporting"
    ),
    value = value * 10000
  )

# Color scheme
colors <- c("International" = "chartreuse4", "Domestic" = "blue", "Regional" = "darkred")

# Single plot with facet_wrap
indonesia_plot <- ggplot(indonesia_long, aes(x = date, y = value, 
                                             group = source_type, color = source_type)) +
  geom_line() +
  facet_wrap(~metric, ncol = 3, scales = "free_y") +
  scale_color_manual(name = "Source Type:", values = colors) +
  labs(title = "Indonesia 2024 Case Study", 
       y = "Articles per 10,000", 
       x = "") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    strip.text = element_text(hjust = 0.5),
    legend.position = c(.87, .25)
  )

ggsave(file.path(output_path, "indonesia_int_vs_local.jpg"), 
       plot = indonesia_plot, height = 5, width = 7)


# === GENERATE INTERNATIONAL SHARE ANALYSIS ===

# Combine international and regional as non-local
data$non_local <- ifelse(data$international == 1 | data$regional == 1, 1, 0)

# Aggregate by non-local flag
data_aggregated <- aggregate(cbind(arrest, protest, legalaction, disaster, censor, electionactivity, 
                                 electionirregularities, activism, martiallaw, cooperate, coup, 
                                 violencenonlethal, violencelethal, corruption, legalchange, 
                                 mobilizesecurity, purge, threaten, raid, defamationcase, 
                                 total_articles, total_articles_country, counts_combinedtotal_articles_civic) ~ non_local, 
                            data = data, FUN = sum)

# Calculate correlations between international and local coverage by country
countries_unique <- unique(data$country)
event_vars <- c("arrest", "protest", "legalaction", "disaster", "censor", 
               "electionactivity", "electionirregularities", "activism", 
               "martiallaw", "cooperate", "coup", "violencenonlethal", 
               "violencelethal", "corruption", "legalchange", "mobilizesecurity", 
               "purge", "threaten", "raid", "defamationcase")

# Filter to only use event vars that exist in the data
event_vars <- intersect(event_vars, names(data))

correlations <- matrix(NA, nrow = length(countries_unique), ncol = length(event_vars))

for(i in seq_along(countries_unique)) {
  country_data <- data[data$country == countries_unique[i], ]
  for(j in seq_along(event_vars)) {
    local_data <- country_data[country_data$non_local == 0, event_vars[j]]
    intl_data <- country_data[country_data$non_local == 1, event_vars[j]]
    
    # Ensure we have data for both local and international, and same length
    if(length(local_data) > 0 && length(intl_data) > 0 && length(local_data) == length(intl_data)) {
      cor_val <- cor(local_data, intl_data, use = "complete.obs")
      correlations[i, j] <- ifelse(is.na(cor_val), 0, cor_val)
    } else if(length(local_data) > 0 && length(intl_data) > 0) {
      # If different lengths, correlate the time series by matching dates
      local_df <- country_data[country_data$non_local == 0, c("date", event_vars[j])]
      intl_df <- country_data[country_data$non_local == 1, c("date", event_vars[j])]
      merged_df <- merge(local_df, intl_df, by = "date", suffixes = c("_local", "_intl"))
      
      if(nrow(merged_df) > 1) {
        cor_val <- cor(merged_df[, paste0(event_vars[j], "_local")], 
                      merged_df[, paste0(event_vars[j], "_intl")], 
                      use = "complete.obs")
        correlations[i, j] <- ifelse(is.na(cor_val), 0, cor_val)
      }
    }
  }
}

# Calculate average correlations
average_cor <- colMeans(correlations, na.rm = TRUE)

# Calculate topic shares
topic_share <- data_aggregated
for(i in names(topic_share)) {
  if(is.numeric(topic_share[[i]]) && i != "non_local") {
    topic_share[[i]] <- topic_share[[i]] / sum(topic_share[[i]], na.rm = TRUE)
  }
}

# Create renamed columns for plotting
topic_names <- c(
  "arrest" = "Arrests", "activism" = "Activism", "protest" = "Protests",
  "legalaction" = "Legal Actions", "disaster" = "Disasters", "censor" = "Censorship",
  "electionactivity" = "Election Activity", "electionirregularities" = "Election Irregularities",
  "martiallaw" = "Martial Law", "cooperate" = "Cooperation", "coup" = "Coup",
  "violencelethal" = "Lethal Violence", "violencenonlethal" = "Non-lethal Violence",
  "corruption" = "Corruption", "legalchange" = "Legal Changes",
  "mobilizesecurity" = "Mobilize Security", "purge" = "Purges",
  "threaten" = "Threats", "raid" = "Raids", "defamationcase" = "Defamation Cases"
)

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
    local_val <- topic_share[topic_share$non_local == 0, var_name]
    intl_val <- topic_share[topic_share$non_local == 1, var_name]
    cor_val <- average_cor[match(var_name, event_vars)]
    
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
    source_type == "local" ~ "National",
    source_type == "international" ~ "International"
  ))

# Order by correlation
plot_data_long$variable <- factor(plot_data_long$variable, 
                                 levels = unique(plot_data$variable[order(plot_data$average_cor)]))

# Create the plot
colors <- c("National" = "blue", "International" = "chartreuse4")

# Format correlation labels (remove leading zero)
plot_data_long$cor_label <- formatC(plot_data_long$average_cor, digits = 2, format = "f")
plot_data_long$cor_label <- str_replace(plot_data_long$cor_label, "^0\\.", ".")
plot_data_long$cor_label <- str_replace(plot_data_long$cor_label, "^-0\\.", "-.")

int_share_plot <- ggplot(plot_data_long, aes(x = variable)) +
  geom_bar(aes(y = value, fill = source_type), stat = "identity", position = "fill", alpha = 0.5) +
  labs(y = "Share", x = "", title = "International Share of Normalized Articles With Correlations") +
  scale_fill_manual(name = "Source Type:", values = colors) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 90),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y = average_cor + 0.15, label = cor_label), vjust = 1, color = "black") +
  geom_point(aes(y = average_cor), color = "darkred")

ggsave(file.path(output_path, "int_share_norm.jpg"), 
       plot = int_share_plot, height = 5, width = 7)

message("Saved: int_share_norm.jpg")

# === GENERATE EVENT SHARE ANALYSIS ===
message("Generating event share analysis...")

# Calculate mean shares by source type
data_temp <- data
available_vars <- intersect(c("arrest", "protest", "legalaction", "disaster", "censor", "electionactivity", 
                             "electionirregularities", "activism", "martiallaw", "cooperate", "coup", 
                             "violencenonlethal", "violencelethal", "corruption", "legalchange", 
                             "mobilizesecurity", "purge", "threaten", "raid", "defamationcase"), 
                           names(data_temp))

event_share <- data_temp %>%
  select(all_of(c("non_local", available_vars))) %>%
  group_by(non_local) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame()

# Topic names for display
topic_names <- c(
  "arrest" = "Arrests", "activism" = "Activism", "protest" = "Protests",
  "legalaction" = "Legal Actions", "disaster" = "Disasters", "censor" = "Censorship",
  "electionactivity" = "Election Activity", "electionirregularities" = "Election Irregularities",
  "martiallaw" = "Martial Law", "cooperate" = "Cooperation", "coup" = "Coup",
  "violencelethal" = "Lethal Violence", "violencenonlethal" = "Non-lethal Violence",
  "corruption" = "Corruption", "legalchange" = "Legal Changes",
  "mobilizesecurity" = "Mobilize Security", "purge" = "Purges",
  "threaten" = "Threats", "raid" = "Raids", "defamationcase" = "Defamation Cases"
)

# Prepare plotting data
plot_data <- data.frame(
  variable = character(0),
  local = numeric(0),
  international = numeric(0)
)

for(i in seq_along(topic_names)) {
  var_name <- names(topic_names)[i]
  if(var_name %in% names(event_share)) {
    local_val <- event_share[event_share$non_local == 0, var_name]
    intl_val <- event_share[event_share$non_local == 1, var_name]
    
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

colors <- c("National" = "blue", "International" = "chartreuse4")

event_share_plot <- ggplot(plot_data, aes(x = variable)) +
  geom_bar(aes(y = international * 10000, fill = "International"), 
           stat = "identity", alpha = 0.5) +
  geom_bar(aes(y = local * 10000, fill = "National"), 
           stat = "identity", alpha = 0.2) +
  labs(y = "Articles per 10,000", x = "", 
       title = "Event Share of Total by International and National") +
  scale_fill_manual(name = "Source Type:", values = colors) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 90),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(output_path, "event_share_total.jpg"), 
       plot = event_share_plot, height = 5, width = 7)

message("Saved: event_share_total.jpg")

# === GENERATE CORRELATION BY COUNTRY ANALYSIS ===
message("Generating country correlation analysis...")

# Calculate country-level correlations and international article counts
countries_unique <- unique(data$country)
country_stats <- data.frame(
  country = countries_unique,
  correlation = numeric(length(countries_unique)),
  int_articles = numeric(length(countries_unique)),
  log_int_articles = numeric(length(countries_unique))
)

for(i in seq_along(countries_unique)) {
  country_data <- data[data$country == countries_unique[i], ]
  
  # Calculate correlation across all civic events
  local_civic <- country_data[country_data$non_local == 0, "civic"]
  intl_civic <- country_data[country_data$non_local == 1, "civic"]
  
  if(length(local_civic) > 0 && length(intl_civic) > 0) {
    cor_val <- cor(local_civic, intl_civic, use = "complete.obs")
    country_stats$correlation[i] <- ifelse(is.na(cor_val), 0, cor_val)
  }
  
  # Calculate international article counts
  intl_data <- country_data[country_data$non_local == 1, ]
  if(nrow(intl_data) > 0) {
    # Adjust for double counting (divide by 2)
    cs_999_col <- ifelse("cs_999" %in% names(intl_data), "cs_999", "total_articles")
    if(cs_999_col == "cs_999") {
      int_count <- sum(intl_data$total_articles_country * (intl_data$total_articles - intl_data$cs_999), na.rm = TRUE) / 2
    } else {
      int_count <- sum(intl_data$total_articles_country * intl_data$total_articles, na.rm = TRUE) / 2
    }
    country_stats$int_articles[i] <- int_count
    country_stats$log_int_articles[i] <- log(max(int_count, 1))  # Avoid log(0)
  }
}

# Create scatter plot
country_plot <- ggplot(country_stats, aes(x = log_int_articles, y = correlation)) +
  geom_point(aes(color = country)) +
  labs(y = "Correlation", x = "Log International Civic Articles", 
       title = "Total International Articles vs National/International Correlation by Country") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-0.3, 0.5))

# Add labels for specific countries
highlight_countries <- c("Timor Leste", "India", "Turkey", "Kosovo")
highlight_data <- country_stats[country_stats$country %in% highlight_countries, ]

if(nrow(highlight_data) > 0) {
  country_plot <- country_plot +
    geom_text(data = highlight_data, 
              aes(log_int_articles, correlation, label = country),
              position = position_dodge(width = 1), vjust = -0.5)
}

ggsave(file.path(output_path, "int_nat_corr_by_country.jpg"), 
       plot = country_plot, height = 5, width = 7)

message("Saved: int_nat_corr_by_country.jpg")

# === ANALYSIS COMPLETE ===
message("=== Source Comparison Analysis Complete ===")