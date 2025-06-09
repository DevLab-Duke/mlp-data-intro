# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains the academic paper data and code for "Tracking Civic Space in Developing Countries with a High-Quality Corpus of Domestic Media and Transformer Models". It hosts everything associated with the MLP (Machine Learning for Peace) data introduction paper.

## Key Architecture

### Data Structure
- **`data/counts/`**: Raw and normalized article counts by category and country-month
  - `full-data.rds` and `full-data.csv`: Complete dataset across all countries
  - `[countryname].csv`: Per-country data files
  - Variables with `Norm` suffix: Raw counts normalized by `article_total`
  - Variables with `_ncr` suffix: Non-politically relevant events filtered out
- **`data/shocks/`**: Event detection algorithm results showing major reporting jumps
- **`data/0-civic-by-source/`** and **`data/0-rai-by-source-and-influencer/`**: Source-level data
- **`data/1-civic-aggregate/`** and **`data/1-rai-aggregate/`**: Aggregated country-month data

### Core Scripts
- **`build_data/build_data.R`**: Main data construction pipeline
- **`build_data/constants.R`**: Event categories, country lists, and source definitions
- **`build_data/mlp_functions.R`**: Custom functions for data extraction and aggregation
- **`data_update.R`**: Updates data from ML4P forecasting repository

### Event Categories
- **Civic events**: 20 categories including arrest, censor, protest, activism, coup, etc.
- **RAI events**: 22 categories of international influence including arms transfers, diplomatic activities, economic aid
- **CR variables**: Subset of civic events that are considered civil rights related

## Common Development Tasks

### Data Processing
```r
# Run main data construction pipeline
source("build_data/build_data.R")

# Update data from forecasting repo
source("data_update.R")
```

### Analysis and Visualization
```r
# R Markdown documents for specific analyses:
# - ICR_Human_Coding.Rmd: Training data inter-coder reliability
# - writing/international_vs_national/international_vs_national_graphs.Rmd
# - writing/shock_detection/shock_detection_figure.Rmd
# - geoparsing_test/Geoparsing_report.Rmd

# Render Quarto documents
quarto render writing/appendix.qmd
```

### Key R Libraries
Core dependencies include: `tidyverse`, `here`, `psych`, `gt`, `kableExtra`, `ggplot2`, `changepoint`

## Country Coverage
The dataset covers 67 developing countries across regions: Eastern Europe/Central Asia, MENA, Latin America & Caribbean, East Asia, Sub-Saharan Africa.

## Source Management
- **International sources**: 16 major outlets (BBC, Reuters, NYT, etc.)
- **Regional sources**: 12 regional outlets
- **Local sources**: Country-specific outlets defined in `constants.R`
- Source entry/exit tracking in final dataset columns for composition change detection

## Important Notes
- All countries have data through 2024-12-01
- Date folder overrides can be specified in `date_folder()` function
- Path configurations in `data_update.R` handle multiple user environments
- Russian and Chinese influence measures available across 22 event categories