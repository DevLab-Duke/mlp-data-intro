#
#   Functions that return various paths, depending on user/system
#


# From: update-forecasts.R ------------------------------------------------


# Optionally override which date folder is used for a country's Civic or RAI
# counts.
# To add an override, expand the override list in the code below
#
date_folder <- function(country, type = c("civic", "rai")) {
  # default: "latest"
  ret <- "latest"
  
  # SPECIFY OVERRIDES HERE, like the example
  # the second list key must always be either "civic" or "rai"
  override <- list()
  
  # Example:
  override[["Belarus"]][["civic"]] <- "2025_1_8"
  override[["Belarus"]][["rai"]] <- "2025_1_8"
  
  
  # check for overrides
  if (country %in% names(override)) {
    if (type %in% names(override[[country]])) {
      ret <- override[[country]][[type]]
      cat(sprintf("Using '%s' subfolder for %s counts\n", ret, toupper(type)))
    }
  }
  
  ret
}


# Civic -------------------------------------------------------------------


# From: paths.R -----------------------------------------------------------

#' Path finders
#'
#' Functions that find the local computer path to various locations
#'
#' @param \dots Combined with the root path using [file.path()].
#'
#' @export
#' @aliases path_root path_dropbox path_datastore
path <- function(...) {
  message("This is just a placeholder for the R doc, use one of the other path functiongs, see ?path")
  invisible(TRUE)
}


#' @describeIn path Path to the ML4P-Civic-Space-Forecasting folder (that is on git).
#' @export
path_root <- function(...) {
  if (Sys.info()["nodename"]=="jeremy-System-Product-Name") {
    base <- "/home/jeremy/Dropbox/github-repos/ML4P-Civic-Space-Forecasting"
  }
  
  # Check to make sure the directory exists and is correct
  if (!dir.exists(base)) {
    stop("Go edit path_root in R/paths.R and add your path to the ML4P-Civic-Space-Forecasting folder")
  }
  
  file.path(base, ...)
}

#' @describeIn path Path to the "ML for Peace" Dropbox folder
#' @export
path_dropbox <- function(...) {
  # default guess
  base <- "~/Dropbox/ML for Peace"
  
  # jeremy: office windows
  if (Sys.info()["user"]=="jerem") {
    base <- "C:/Users/jerem/Dropbox/ML for Peace"
  }
  # jeremy: home linux
  if (Sys.info()["nodename"]=="jeremy-System-Product-Name") {
    base <- "/home/jeremy/Dropbox/ML for Peace"
  }
  # Check to make sure the directory exists and is correct
  if (!dir.exists(base)) {
    stop("Go edit path_dropbox in R/misc.R and add your path to the ML4P Dropbox folder")
  }
  
  file.path(base, ...)
}


#' @describeIn path Path to the civic counts folder
#' @export
path_counts_civic <- function(...) {
  path_dropbox("Counts_Civic_New", ...)
}


# From: extract-counts-by-source.R ----------------------------------------

# Read and format a raw Counts_... file from Dropbox
# Handles some minor annoyances.
read_counts_csv <- function(file) {
  df <- suppressMessages(readr::read_csv(file, show_col_types = FALSE))
  df$source <- tools::file_path_sans_ext(basename(file))
  
  # Fix a date column, indexing months to first, not last date
  df$date <- df[[colnames(df)[grepl("date", colnames(df))][1]]]
  df$date <- as.Date(paste0(substr(df$date, 1, 8), "01"))
  
  # Remove other date info we don't need
  df$date...1 <- df$date...2 <- df$...1 <- df$date.1 <- NULL
  df$year <- df$month <- NULL
  
  # fix names and bring source, date columns to front
  front <- c("source", "date")
  df <- df[, c(front, setdiff(colnames(df), front))]
  
  df
}


# this function whitelists and combines all the [source].csv files in a folder.
# low-level function that is used by the three specific extractors below
vet_and_combine_sources <- function(path, country) {
  csv_files <- dir(path, full.names = TRUE, pattern = ".csv$")
  
  # White list sources
  valid_sources <- vet_sources(sources = basename(csv_files),
                               country = country)
  not_used_sources <- basename(csv_files)[!basename(csv_files) %in% valid_sources]
  csv_files <- csv_files[basename(csv_files) %in% valid_sources]
  
  df_list <- lapply(csv_files, read_counts_csv)
  df <- do.call(rbind, df_list)
  
  # Add country columns
  df <- cbind(country = country, df)
  attr(df, "used_sources") <- valid_sources
  attr(df, "not_used_sources") <- not_used_sources
  df
}


#' Resolve subfolder
#'
#' Resolve the date subfolder name for a country on Dropbox
#'
#' @param date One of "latest", "all", or a specific folder name like "2022_1_12".
#'
#' @returns A character vector of sub-folder names.
#'
#' @keywords internal
db_resolve_date <- function(type = c("rai", "civic"), country, date = "latest") {
  type <- match.arg(type)
  
  if (type=="rai") {
    folders <- dir(path_counts_rai(country))
  } else {
    folders <- dir(path_counts_civic(country))
  }
  
  # take out any non-standard folders, i.e. keep only ones containing numbers
  # and "_". This is to deal with Serbia/(cliff)2021_12_15
  folders <- folders[grepl("^[0-9_]+$", folders)]
  
  # the "kind-of-date" format is not necessarily in the correct order because
  # the month and day integers don't aren't padded with 0, i.e. 2023_4_7 comes
  # after 2023_4_13 in string sorting, but should be before/earlier
  dates <- as.Date(folders, format = "%Y_%m_%d")
  folders <- folders[order(dates)]
  
  if (date=="latest") {
    res <- tail(folders, 1)
  } else if (date=="all") {
    res <- folders
  } else {
    res <- date
    if (!res %in% folders) {
      msg <- sprintf("Folder '%s' does not exist, found: '%s'",
                     date,
                     paste0(folders, collapse = "', '"))
      stop(msg)
    }
  }
  res
}


extract_civic_counts_by_source <- function(country, date = "latest", quiet = FALSE) {
  stopifnot(
    "country must be just 1 country" = length(country)==1
  )
  
  date_folder <- db_resolve_date("civic", country, date)
  if (!quiet) {
    cat(paste0("Using '", date_folder, "/' folder\n"))
  }
  date_folder_path <- path_counts_civic(country, date_folder)
  
  # Path routing to accommodate legacy folder structures
  # The date folder has Civic_Related, Combined, Non_Civic_Related subfolder
  if ("Civic_Related" %in% dir(date_folder_path)) {
    combined <- vet_and_combine_sources(file.path(date_folder_path, "Combined"),
                                        country)
    combined_not_used_sources <- attr(combined, "not_used_sources")
    # fix -999 name
    names(combined)[names(combined) %in% "-999"] = "cs_999"
    original_column_order = colnames(combined)
    combined <- combined[, setdiff(colnames(combined), cr_vars)]
    
    csr <- vet_and_combine_sources(file.path(date_folder_path, "Civic_Related"),
                                   country)
    csr_not_used_sources <- attr(csr, "not_used_sources")
    csr <- csr[, c("country", "source", "date", cr_vars)]
    
    ncr <- vet_and_combine_sources(file.path(date_folder_path, "Non_Civic_Related"),
                                   country)
    ncr_not_used_sources <- attr(ncr, "not_used_sources")
    ncr <- ncr[, c("country", "source", "date", cr_vars)]
    idx <- colnames(ncr) %in% cr_vars
    colnames(ncr)[idx] <- paste0("ncr_", colnames(ncr)[idx])
    
    df <- dplyr::inner_join(combined, csr, by = c("country", "source", "date")) |>
      dplyr::inner_join(ncr, by = c("country", "source", "date"))
    df <- df[, c(original_column_order, setdiff(colnames(df), original_column_order))]
    
    # recreate the "not_used_sources" attribute
    attr(df, "not_used_sources") <- unique(c(combined_not_used_sources,
                                             csr_not_used_sources,
                                             ncr_not_used_sources))
    
  } else {
    stop("Unexpected folder dropbox civic counts folder structure.")
  }
  
  # Write to datastore
  outfile <- here("data", "0-civic-by-source", sprintf("%s.csv", country))
  readr::write_csv(df, file = outfile, progress = FALSE)
  
  invisible(df)
}


read_civic_by_source <- function(country) {
  path <- here("data", "0-civic-by-source", sprintf("%s.csv", country))
  res <- readr::read_csv(path, show_col_types = FALSE)
  # this removes the spec attribute
  tibble::as_tibble(res)
}


# From: aggregate-and-merge.R ---------------------------------------------


#' Create country-month Civic and RAI counts
#'
#' Aggregate and merge the Civic and RAI data for a country to produce
#' unnormalized country-month data. These are written to `{datastore}/1-raw-counts`.
#'
#' @param country Country
#' @param quiet Print diagnostic messages?
#'
#' @details Depends on the 0-civic.../0-rai... CSV files in the datastore,
#'   and the assets/impute-list.csv file.
#'
#' @return The new data are saved to the datastore and invisible returned as
#' well.
#'
#' @export
#' @aliases read_raw_counts update_story_counts update_source_entries read_source_entries
aggregate_and_merge <- function(country, quiet = TRUE) {
  df <- read_civic_by_source(country)
  # Process Civic variables:
  df$source <- NULL
  df <- df |>
    dplyr::group_by(country, date) |>
    dplyr::summarise_all(sum) |>
    dplyr::ungroup()
    
  # Re-order columns
  new_cols <- c("country", "date",
                civic, "cs_999",
                paste0("ncr_", cr_vars),
                "total_articles")
  df <- df[, new_cols]
  
  # Check for missing values
  any_na <- !all(complete.cases(df))
  if (any_na) {
    warning(sprintf("Merged data for '%s' has missing values.", country))
  }
  if (any_na & !quiet) {
    cat(sprintf("Merged data for '%s' has missing values\n", country))
  }
  
  # Call country_last_month() from constants.R that lists the last month of good data for each country
  df <- df %>% dplyr::filter(date <= country_last_month(!!country))
  
  # Add normalized variables to the master dataframe
  df[, paste0(civic , "Norm")] <- as.data.frame(lapply(df[,civic], function(x) x/df[, "total_articles"]))
  df[, paste0( paste0("ncr_", cr_vars) , "Norm")] <- as.data.frame(lapply(df[, paste0("ncr_", cr_vars) ], function(x) x/df[, "total_articles"]))
  
  out_path <- here("data", "1-civic-aggregate", sprintf("%s.csv", country))
  readr::write_csv(df, out_path)
  
  invisible(df)
}

#' @describeIn aggregate_and_merge Mashes together all of the by-source data to
#' create the source entries matrix.
#' @export
update_source_entries <- function() {
  # mash everything in 0-...-by-source together (both Cicic and RAI)
  files <- dir(here("data","0-civic-by-source"), full.names = TRUE)
  civic <- lapply(files, readr::read_csv, show_col_types = FALSE, progress = FALSE)
  civic <- dplyr::bind_rows(civic)
  
  files <- dir(here("data","0-rai-by-source-and-influencer"), full.names = TRUE)
  rai <- lapply(files, readr::read_csv, show_col_types = FALSE)
  rai <- dplyr::bind_rows(rai)
  
  rai <- rai[rai$influencer=="Combined", ]
  rai$influencer <- NULL
  
  raw <- dplyr::inner_join(civic, rai, by = c("country", "date", "source"))
  
  # record the full date range here because I'm going to alter raw below
  full_date_range <- sort(unique(raw$date))
  
  # Identify new sources that enter mid-time-series and store the last zero month
  # We want the last non-zero month instead of the first non-zero month because
  # most of our sources come online intermittently and have an article or two before
  # they really start publishing. This flag is to identify when we expect the source
  # to really affect the composition of counts, rather than first publication date
  
  # Remove international and regional sources from list of all sources, we're only
  # interested in local sources (keep regional sources that double as local)
  remove_sources <- c(ml4p.forecast::isources, ml4p.forecast::rsources[! ml4p.forecast::rsources %in%  c("balkaninsight.com.csv", "indiatimes.com.csv", "timesofindia.indiatimes.com.csv", "theeastafrican.co.ke.csv") ] )
  remove_sources <- gsub(".csv", "", remove_sources)
  lsources_all <- setdiff(unique(raw$source), remove_sources)
  raw <- raw[raw$source %in% lsources_all, ]
  
  # Calculate total articles per month. Once all countries have the total_articles variable, we can remove this and use that variable
  sum_cols <- names(raw)[!names(raw) %in% c("country", "source", "date", "rai_999")]
  raw$total <- rowSums(raw[, sum_cols], na.rm = TRUE)
  
  # Changed 5/26/2023:
  # New method: We now create binary indicators for each source that turn-on
  # in all months where the source has volume > 0
  # Old method: We want to find the last date a source had a total of 0,
  # and then create a new date x source matrix that has values of 0 before that
  # last date, and values of 1 after it (indicating good coverage).
  
  last0 <- raw |>
    dplyr::select(country, source, date, total) |>
    dplyr::mutate(total =  dplyr::case_when(total > 0 ~ 1, TRUE ~ 0) ) |>
    dplyr::select(source, date, total)
  
  # Make a matrix of date x source, where values are total
  skeleton <- tibble::tibble(date = full_date_range)
  skeleton[, unique(last0$source)] <- 0L
  
  for (ss in unique(last0$source)) {
    last_date <- last0$date[last0$source==ss & last0$total == 1]
    skeleton[[ss]][skeleton$date %in% last_date] <- 1L
  }
  saveRDS(skeleton, here("data","1-source_entries.rds"))
  invisible(skeleton)
}

# From: whitelist-sources.R -----------------------------------------------

#' Country Source Whitelist
#'
#' Whitelist of all usable sources for a country---international, regional, and
#'   local.
#'
#' @param country Country
#' @param sources A list of candidate sources.
#'
#' @export
#' @aliases isources rsources local_source_select vet_sources
whitelist_sources <- function(country) {
  stopifnot(length(country)==1)
  c(
    isources,
    rsources,
    local_source_select(country)$lsources
  )
}

#' @describeIn whitelist_sources Vet a list of sources against whitelist; returns
#'   subset of valid sources.
#' @export
vet_sources <- function(sources, country) {
  valid <- whitelist_sources(country)
  valid_sources <- intersect(sources, valid)
  valid_sources
}



# RAI ---------------------------------------------------------------------

#' @describeIn path Path to the RAI counts folder
#' @export
path_counts_rai <- function(...) {
  path_dropbox("Counts_RAI", ...)
}

#' Read un-aggregated RAI counts
#'
#' Read the un-aggregated RAI counts by source and influencer in the datastore.
#' Exported for rai.atari.
#'
#' @param country Country
#'
#' @export
read_rai_by_source_and_influencer <- function(country) {
  path <- here("data","0-rai-by-source-and-influencer", sprintf("%s.csv", country))
  res <- readr::read_csv(path, show_col_types = FALSE)
  # this removes teh spec attribute
  tibble::as_tibble(res)
}

#' Country RAI Counts by Source and Influencer
#'
#' Extract RAI counts by source and influencer for a country from Dropbox. This
#' function literally just combines a bunch of separate CSV files into one
#' big CSV file of raw counts for one country. It does not do source whitelisting.
#'
#' @param country Country name
#' @param date character; name of the sub-folder to read; defaults to latest available
#'
#' @returns Invisibly returns the raw RAI counts by news source, but the main
#'   effect is to write a CSV to the datastore on Dropbox.
#'
#' @export
#' @seealso [extract_article_total()], [extract_rai_counts_by_source_and_influencer()]
extract_rai_counts_by_source_and_influencer <- function(country,
                                                        date = "latest",
                                                        quiet = TRUE) {
  stopifnot(
    "country must be just 1 country" = length(country)==1
  )
  
  date_folder <- db_resolve_date("rai", country, date)
  if (!quiet) {
    cat(paste0("Using '", date_folder, "/' folder\n"))
  }
  
  influencer_ops <- c("Combined", "China", "Russia")
  
  # The date folder should contain Combined, Russia, China sub-folders
  if (!all(influencer_ops %in% dir(path_counts_rai(country, date_folder)))) {
    stop("could not find influencer folders")
  }
  
  df_by_influencer <- list()
  for (influencer_i in influencer_ops) {
    
    path <- path_counts_rai(country, date_folder, influencer_i)
    
    df <- vet_and_combine_sources(path, country)
    
    # fix -999 name
    names(df)[names(df) %in% "-999"] = "rai_999"
    
    # Add the influencer type and move to front
    df$influencer <- influencer_i
    front <- c("influencer")
    df <- df[, c(front, setdiff(colnames(df), front))]
    
    df_by_influencer[[influencer_i]] <- df
  }
  df_by_influencer <- do.call(rbind, df_by_influencer)
  rownames(df_by_influencer) <- NULL
  
  
  # Retrieve `total_articles` from civic counts
  dft = read_csv(here("data", "0-civic-by-source", sprintf("%s.csv", country)), show_col_types = FALSE) %>%
    dplyr::select(country, source, date, total_articles)
  stopifnot(
    "rai and civic row numbers are not equal" = nrow(dft)==(nrow(df_by_influencer)/3)
  )
  
  df_by_influencer = left_join(df_by_influencer, dft)

  # Write to datastore
  outfile <- here("data", "0-rai-by-source-and-influencer", sprintf("%s.csv", country))
  readr::write_csv(df_by_influencer, file = outfile, progress = FALSE)
  
  invisible(df_by_influencer)
}


aggregate_and_merge_rai <- function(country, quiet = TRUE) {
  df <- read_rai_by_source_and_influencer(country)
  # Process RAI variables:
  df$source <- NULL
  df <- df |>
    dplyr::group_by(influencer, country, date) |>
    dplyr::summarise_all(sum) |>
    dplyr::ungroup()
  
  
  # Re-order columns.
  # This will also drop "social_academic_cultural_activity" and
  # "official_security_force_facility_presence"
  new_cols <- c("country", "influencer", "date",
                rai, "rai_999",
                "total_articles")
  df <- df[, new_cols]
  
  # Check for missing values
  any_na <- !all(complete.cases(df))
  if (any_na) {
    warning(sprintf("Merged data for '%s' has missing values.", country))
  }
  if (any_na & !quiet) {
    cat(sprintf("Merged data for '%s' has missing values\n", country))
  }
  
  # Call country_last_month() from constants.R that lists the last month of good data for each country
  df <- df %>% dplyr::filter(date <= country_last_month(!!country))
  
  # Add normalized variables to the master dataframe
  df[, paste0(rai , "Norm")]   <- as.data.frame(lapply(df[,rai], function(x) x/df[, "total_articles"]))
  
  out_path <- here("data", "1-rai-aggregate", sprintf("%s.csv", country))
  readr::write_csv(df, out_path)
  
  invisible(df)
}
