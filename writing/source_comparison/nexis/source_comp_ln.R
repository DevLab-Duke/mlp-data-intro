library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(ml4p.forecast)

# Read-in MLP source list
source(here("tracker", "resources", "source_comparison", "source_list.R")) 
dat = dat |>
  rename(country = Name)

# Read-in nexis uni sources
ln = read.csv(here("tracker", "resources", "source_comparison", "Nexis Uni -- Content Listing--non-US (2023).csv"))

# List mlp countries
mlp_countries = unique(dat$country)

# Subset common countries
ln = ln |>
  mutate(Country.of.Origin = case_when(Country.of.Origin == "Congo, Democratic Republic of" ~ "DR Congo", TRUE ~ Country.of.Origin )) |> 
  filter( Country.of.Origin %in% mlp_countries) 

# List mlp countries not in LN
nl_missing_countries = mlp_countries[!mlp_countries %in% ln$Country.of.Origin]

# Compare the number of languages covered
length(table(ln$Language))

# Compare the median sources per country
ln_tab = ln |> 
  group_by(Country.of.Origin) |>
  summarise(n = n() )

mlp_tab = dat |>
  group_by(country) |>
  summarise(n = n() )

median(ln_tab$n)
median(mlp_tab$n)
rm(ln_tab, mlp_tab)

# Coverage over time
ln = ln  |>
  mutate(
    First_Year = as.numeric(sub(".*?(\\d{4}).*", "\\1", Coverage.Dates)),
    Last_Year = case_when(
      grepl("current$", Coverage.Dates, ignore.case = TRUE) ~ 2023,
      TRUE ~ as.numeric(sub(".*?(\\d{4}).*?(\\d{4}).*", "\\2", Coverage.Dates))
    )
  ) 

median(ln$First_Year, na.rm = T)
median(ln$Last_Year, na.rm = T)

lnt  = ln |>
  pivot_longer(cols = First_Year:Last_Year) |>
  mutate(name = gsub("_", " ", name) )

lnt %>%
  group_by(name, value) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = value, y = count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(y = "Number of sources") +
  theme(legend.text = element_text(size = 5), legend.title = element_blank(), axis.title.x = element_blank(),
        legend.position = c(.3, .6), legend.box.background = element_rect(colour = "black")) 
ggsave(here("tracker", "resources", "source_comparison", "ln_years.png") , width = 8, height = 6)

## Read-in MLP source-level data
folder_path = "/home/jeremy/Dropbox/ML for Peace/ml4p.forecasting/0-civic-by-source"


# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read and bind all CSV files into a single dataframe
data <- csv_files %>%
  purrr::map_dfr(readr::read_csv) 


mlt = data |>
  mutate(year = as.numeric(format(as.Date(date), "%Y"))) %>%  # Extract the year from the 'date' column
  filter(total_articles > 0) %>%  # Filter rows where total_articles is greater than zero
  group_by(source) %>%
  summarize(First_Year = min(year),
            Last_Year = max(year))  # Find the minimum year for each source where articles > 0

mlt %>%
  pivot_longer(cols = First_Year:Last_Year) |>
  mutate(name = gsub("_", " ", name) ) |>
  group_by(name, value) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = value, y = count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ name, scales = "free", nrow = 2) +
  theme_bw() +
  labs(y = "Number of sources") +
  scale_x_continuous( breaks= 2012:2023, labels =  2012:2023 ) +
  theme(legend.text = element_text(size = 5), legend.title = element_blank(), axis.title.x = element_blank(),
        legend.position = c(.3, .6), legend.box.background = element_rect(colour = "black")) 
ggsave(here("tracker", "resources", "source_comparison", "mlp_years.png") , width = 8, height = 6)


