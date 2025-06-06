# mlp-data-intro
Repository for: Tracking Civic Space in Developing Countries with a High-Quality Corpus of Domestic Media and Transformer Models

Current Overleaf Location: https://www.overleaf.com/9891685827jxbvnrshzxbw#683bc8

This repo hosts everything associated with the academic paper introducing the MLP data.

There are two subfolders that are relevant:
- `counts\` contains the raw count of articles in each category and the normalized counts. The data is both provided in total (full-data.RDS and full-data.csv) and on a per country basis ([countryname].csv).
  + Articles with the `Norm` suffix are the raw counts divided by the `article_total`, which counts the number of articles published about that country per country-month. These variables are all normalized by total article production in each country-month.
  + For some event categories, we also have variables with an "_ncr" suffix; these are the result of models identifying events that fall under a given event category (i.e. arrest) but are not politically relevant events (i.e. arrest of a petty criminal).
  + There's a lookup table `cs_vars.csv` linking the variable names to their substantive label (used in the paper)
  + There are also measures separating overall Russian and Chinese influence across the 22 event categories (`rus_influence` and `chn_influence`)
  + The appendix.pdf lists the sources we have for each country and region. Sometimes, we have sources that enter late in the time-series or drop out before the end. The final columns in the dataset flags the entry/exit for sources that aren't present for the full time-series. This flags major composition changes for each country.
- `shocks\` contains the results of our event detection algorithm (described in the paper), which detects major jumps in the level of reporting on each event category which should correspond to major events. If you look at any of the recent reports on our website, you can see examples of the shocks we detect. This gives a nice qualitative illustration of the underlying events.

## Count Checking (`count_check`)

- `README.md` contains instructions for checking sources

## Writing (`writing`)

- `PlotsDR` contains Diego's validation plots
- `shock_detection` contains Donald's code for shock detection figures
- `languages` contains Notion export for check number of languages
- `international_vs_national` contains code and output comparing international and national event coverage

## Building the source-level and country-month datasets

`build_data.R` calls a series of functions to build the source-level and country-month datasets. `1-source_entries.rds` captures the months covered by each source. 


## GPT Summarization

## geoparsing_test