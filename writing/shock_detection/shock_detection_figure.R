# Shock Detection Figure Script

library(here)
library(ggplot2)
library(tidyverse)
library(lubridate)

# Function to plot civic event shocks
plot_civic_shock = function(country, event) {
  
  # Define variable
  var = event

  # Read civic variable mapping from cs_vars.csv
  cs_vars = readr::read_csv(here::here("data", "cs_vars.csv"))
  
  # Setup label mapping for civic events from cs_vars.csv
  choices_df = cs_vars %>%
    select(id, names)
  
  # Read combined civic data
  civic_data = readr::read_csv(here::here("data", "final-counts", "full-civic-data.csv"))
  shock_data = readr::read_csv(here::here("data", "final-counts", "shock-civic-data.csv"))
  
  # Filter for specific country
  civic_country = civic_data %>% 
    filter(country == !!country) %>%
    select(date, all_of(var))
  
  shock_country = shock_data %>% 
    filter(country == !!country) %>%
    select(date, var) %>%
    rename_with(~ str_replace(.x, "Norm$", "Shock"), ends_with("Norm"))
    
  
  # Join data
  combined_data = civic_country %>%
    left_join(shock_country, by = "date") %>%
    mutate(date_my = ymd(date))
  
  # Get last 36 months of data
  ends = nrow(combined_data)
  starts = max(1, ends-35)
  pred_data = combined_data[starts:ends,]
  
  # Date breaks
  des_break = pred_data$date_my[c(TRUE, FALSE)]
  
  pred_data = as.data.frame(pred_data)
  
  # Get title
  title = choices_df[choices_df$id == var,]$names
  if(length(title) == 0) title = var  # fallback if not found
  
  if(title %in% c("Civic Space Index")){
    graph_y_lab = paste(title)
    plot_title = paste("Shocks in", country, title)
  } else{
    graph_y_lab = paste("% of Articles on", title)
    plot_title = paste("Shocks in", country, title)
  }

  # Create plot
  a = ggplot(pred_data, aes(x = date_my, y = get(var))) +
    geom_line(linewidth = 1, color = "black", linetype=1) +
    geom_point(data = subset(pred_data, get( str_replace(var, "Norm", "Shock")  ) == 1), 
               aes(x = date_my, y = get(var)), color = "red", size = 2.5) +
    theme_bw() +
    scale_x_date(breaks = des_break, date_labels = "%b %y", 
                 expand = expansion(mult = c(-0.03, .02))) +
    labs(y = graph_y_lab, x = "Date") +
    ggtitle(plot_title) +
    theme(plot.title = element_text(hjust = 0, size = 18),  
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent_format())

  # Save plot with dynamic country name
  filename = paste0(tolower(gsub(" ", "_", country)), "_", gsub("Norm", "", var), "_shock_detection.png")
  ggsave(paste0(here(),"/writing/shock_detection/", filename), 
         plot = a, height = 5, width = 7)
  
  return(a)
}

# Function to plot RAI event shocks
plot_rai_shock = function(country, influencer, event) {
  
  # Define variable
  var = paste0(event, "Norm")  # RAI events have Norm suffix
  var_shk = event  # RAI shock columns don't have Norm suffix
  
  # Read RAI variable mapping from rai_vars.csv
  rai_vars = readr::read_csv(here::here("data", "rai_vars.csv"))
  
  # Setup label mapping for RAI events from rai_vars.csv
  rai_choices_df = rai_vars %>%
    mutate(id = paste0(var, "Norm")) %>%
    select(id, names = name)
  
  # Read combined RAI data  
  rai_data = readr::read_csv(here::here("data", "final-counts", "full-rai-data.csv"))
  shock_data = readr::read_csv(here::here("data", "final-counts", "shock-rai-data.csv"))
  
  # Filter for specific country and influencer
  rai_country = rai_data %>% 
    filter(country == !!country, influencer == !!influencer) %>%
    select(date, all_of(var)) %>%
    mutate(date_my = ymd(date))
  
  shock_country = shock_data %>% 
    filter(country == !!country, influencer == !!influencer) %>%
    select(date, all_of(var_shk)) %>%
    mutate(date_my = ymd(date))
  
  # Join data
  combined_data = rai_country %>%
    left_join(shock_country, by = c("date", "date_my"))
  
  # Get last 36 months of data
  ends = nrow(combined_data)
  starts = max(1, ends-35)
  pred_data = combined_data[starts:ends,]
  
  # Date breaks
  des_break = pred_data$date_my[c(TRUE, FALSE)]
  
  pred_data = as.data.frame(pred_data)
  
  # Get title
  title = rai_choices_df[rai_choices_df$id == var,]$names
  if(length(title) == 0) title = event  # fallback if not found
  
  graph_y_lab = paste("% of Articles on", title)
  plot_title = paste("RAI Shocks in", country, "-", str_to_title(influencer), title)

  # Create plot
  a = ggplot(pred_data, aes(x = date_my, y = get(var))) +
    geom_line(linewidth = 1, color = "black", linetype=1) +
    geom_point(data = subset(pred_data, get(var_shk) == 1), 
               aes(x = date_my, y = get(var)), color = "red", size = 2.5) +
    theme_bw() +
    scale_x_date(breaks = des_break, date_labels = "%b %y", 
                 expand = expansion(mult = c(-0.03, .02))) +
    labs(y = graph_y_lab, x = "Date") +
    ggtitle(plot_title) +
    theme(plot.title = element_text(hjust = 0, size = 18),  
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_y_continuous(labels = scales::percent_format())

  # Save plot with dynamic country and influencer name
  filename = paste0(tolower(gsub(" ", "_", country)), "_", influencer, "_", event, "_shock_detection.png")
  ggsave(paste0(here(),"/writing/shock_detection/", filename), 
         plot = a, height = 5, width = 7)
  
  return(a)
}

# Example usage:
# Generate Indonesia arrest shocks visualization
shock_plot <- plot_civic_shock("Columbia", "arrestNorm")

# Display plot
print(shock_plot)

# Example RAI plot (uncomment to use):
# russia_arms_indonesia <- plot_rai_shock("Indonesia", "russia", "arms_transfer_security_aid_assistance")
# print(russia_arms_indonesia)