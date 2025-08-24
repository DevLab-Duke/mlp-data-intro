#
#   Functions that return various paths, depending on user/system
#


# From: update-forecasts.R ------------------------------------------------


#' Override Date Folder Selection
#'
#' Optionally override which date folder is used for a country's Civic or RAI
#' counts. By default, uses "latest" folder, but specific date folders can be
#' specified for individual countries and data types.
#'
#' @param country Character string. Name of the country.
#' @param type Character string. Type of data, either "civic" or "rai".
#' @return Character string specifying the date folder to use (e.g., "latest", "2025_1_8").
#' @details To add an override, expand the override list in the function code.
#' When an override is specified, the function prints a message indicating
#' which subfolder is being used.
#' @examples
#' # Get default date folder for Nigeria civic data
#' date_folder("Nigeria", "civic")
#' 
#' # Get date folder for Belarus (has override)
#' date_folder("Belarus", "civic")
#' 
#' # Get date folder for RAI data
#' date_folder("Ukraine", "rai")
#' @export
date_folder <- function(country, type = c("civic", "rai")) {
  # default: "latest"
  ret <- "latest"
  
  # SPECIFY OVERRIDES HERE, like the example
  # the second list key must always be either "civic" or "rai"
  override <- list()
  
  # Override list:
  # override[["Belarus"]][["civic"]] <- "2025_1_8"
  # override[["Belarus"]][["rai"]] <- "2025_1_8"
  
  
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

#' Path Finders
#'
#' Functions that find the local computer path to various locations in the
#' MLP project structure. This is the main documentation entry point for all
#' path-related functions.
#'
#' @param \dots Combined with the root path using [file.path()].
#' @return This function just returns TRUE invisibly and is used for documentation.
#' @details This function is a placeholder for documentation purposes. Use the
#' specific path functions like \code{\link{path_root}}, \code{\link{path_dropbox}},
#' or \code{\link{path_counts_civic}} instead.
#' @seealso \code{\link{path_root}}, \code{\link{path_dropbox}}, \code{\link{path_counts_civic}}, \code{\link{path_counts_rai}}
#' @examples
#' # This function is for documentation only
#' # Use specific path functions instead:
#' # path_root("some", "subdirectory")
#' # path_dropbox("Counts_Civic_New")
#' @export
#' @aliases path_root path_dropbox path_datastore
path <- function(...) {
  message("This is just a placeholder for the R doc, use one of the other path functiongs, see ?path")
  invisible(TRUE)
}


#' @describeIn path Path to the ML4P-Civic-Space-Forecasting folder (that is on git).
#' @param \dots Additional path components to append using [file.path()].
#' @return Character string representing the full path to the project root or subdirectory.
#' @details This function constructs paths to the main project directory that
#' contains the git repository. It includes system-specific path configurations
#' and validates that the directory exists.
#' @examples
#' # Get path to project root
#' path_root()
#' 
#' # Get path to subdirectory
#' path_root("data", "civic")
#' 
#' # Get path to specific file
#' path_root("build_data", "constants.R")
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
#' @param \dots Additional path components to append using [file.path()].
#' @return Character string representing the full path to the Dropbox folder or subdirectory.
#' @details This function constructs paths to the main Dropbox folder containing
#' the ML for Peace project data. It includes system-specific path configurations
#' for different users and validates that the directory exists.
#' @examples
#' # Get path to Dropbox root
#' path_dropbox()
#' 
#' # Get path to civic counts folder
#' path_dropbox("Counts_Civic_New")
#' 
#' # Get path to specific country folder
#' path_dropbox("Counts_Civic_New", "Nigeria")
#' @export
path_dropbox <- function(...) {
  # default guess
  base <- "~/Dropbox/ML for Peace"

  # ZR: Mac
  if (Sys.info()[["user"]] == "zungrulin") {       
    base <- "/Users/zungrulin/Library/CloudStorage/Dropbox/ML for Peace"  
  }
  
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
#' @param \dots Additional path components to append using [file.path()].
#' @return Character string representing the full path to the civic counts folder or subdirectory.
#' @details This function constructs paths to the Dropbox folder containing
#' civic event counts data. It's a convenience wrapper around \code{path_dropbox}.
#' @examples
#' # Get path to civic counts root
#' path_counts_civic()
#' 
#' # Get path to specific country
#' path_counts_civic("Nigeria")
#' 
#' # Get path to specific date folder
#' path_counts_civic("Nigeria", "2024_12_1")
#' @export
path_counts_civic <- function(...) {
  path_dropbox("Counts_Civic_New", ...)
}


# From: extract-counts-by-source.R ----------------------------------------

#' Read and Format Raw Counts CSV File
#'
#' Reads and formats a raw counts CSV file from Dropbox, handling various
#' data formatting issues and standardizing the structure.
#'
#' @param file Character string. Path to the CSV file to read.
#' @return Data frame with standardized columns including source, date, and event counts.
#' @details This function handles several data formatting issues:
#' \itemize{
#'   \item{Extracts source name from filename}
#'   \item{Standardizes date column to first day of month}
#'   \item{Removes redundant date columns}
#'   \item{Reorders columns with source and date first}
#' }
#' @examples
#' \dontrun{
#' # Read a civic counts file
#' df <- read_counts_csv("/path/to/bbc.com.csv")
#' 
#' # Check the structure
#' str(df)
#' head(df)
#' }
#' @keywords internal
read_counts_csv <- function(file) {
  df <- suppressMessages(readr::read_csv(file, show_col_types = FALSE))
  df$source <- tools::file_path_sans_ext(basename(file))
  
  # Fix a date column, indexing months to first, not last date
  # df$date <- df[[colnames(df)[grepl("date", colnames(df))][1]]]
  date_col <- colnames(df)[grepl("date", colnames(df), ignore.case = TRUE)]
  if (length(date_col) == 0) {
    stop(sprintf("No date column found in %s", basename(file)))
  }
  df$date <- df[[date_col[1]]]
  df$date <- as.Date(paste0(substr(df$date, 1, 8), "01"))
  
  # Remove other date info we don't need
  df$date...1 <- df$date...2 <- df$...1 <- df$date.1 <- NULL
  df$year <- df$month <- NULL
  
  # fix names and bring source, date columns to front
  front <- c("source", "date")
  df <- df[, c(front, setdiff(colnames(df), front))]
  
  df
}


#' Vet and Combine News Sources
#'
#' Validates and combines all CSV files in a folder against a whitelist of
#' approved sources for a given country. This is a low-level function used
#' by the specific data extraction functions.
#'
#' @param path Character string. Path to the folder containing CSV files.
#' @param country Character string. Name of the country for source validation.
#' @param use_region_filter Logical. If TRUE, apply regional filtering to sources.
#' @return Data frame with combined data from all valid sources, plus attributes
#' for source validation results.
#' @details This function:
#' \itemize{
#'   \item{Validates sources against whitelist}
#'   \item{Warns about missing or unexpected local sources}
#'   \item{Combines all valid source files}
#'   \item{Adds country column}
#'   \item{Stores validation results as attributes}
#' }
#' @examples
#' \dontrun{
#' # Combine sources for Nigeria
#' df <- vet_and_combine_sources("/path/to/Nigeria/Combined", "Nigeria")
#' 
#' # Check validation results
#' attr(df, "used_sources")
#' attr(df, "not_used_sources")
#' }
#' @keywords internal
vet_and_combine_sources <- function(path, country, use_region_filter = FALSE) {
  csv_files <- dir(path, full.names = TRUE, pattern = "\\.csv$")
  
  # Get expected sources from constants.R
  expected_sources <- whitelist_sources(country, use_region_filter = use_region_filter)
  
  # White list sources
  valid_sources <- vet_sources(sources = basename(csv_files),
                               country = country,
                               use_region_filter = use_region_filter)
  not_used_sources <- basename(csv_files)[!basename(csv_files) %in% valid_sources]
  csv_files <- csv_files[basename(csv_files) %in% valid_sources]
  
  # Check for source discrepancies and generate warnings (only for local sources)
  sources_in_folder <- basename(dir(path, pattern = "\\.csv$"))
  
  # Get local sources only for warning checks
  local_sources_expected <- local_source_select(country)$lsources
  local_sources_in_folder <- intersect(sources_in_folder, local_sources_expected)
  
  # Filter to only check local sources for discrepancies
  missing_local_sources <- setdiff(local_sources_expected, sources_in_folder)
  unexpected_local_sources <- setdiff(sources_in_folder, expected_sources)
  # Remove international and regional sources from unexpected list
  international_and_regional <- c(isources, rsources)
  unexpected_local_sources <- setdiff(unexpected_local_sources, international_and_regional)
  
  # Warn about local sources expected but missing
  if (length(missing_local_sources) > 0) {
    warning(sprintf("Country '%s': %d local source(s) defined in constants.R but missing from dropbox folder:\n  %s", 
                   country, length(missing_local_sources), paste(missing_local_sources, collapse = ", ")), 
           call. = FALSE)
  }
  
  # Warn about local sources present but not expected
  # Filter out sapo.pt.csv from unexpected sources before warning
  unexpected_local_sources <- setdiff(unexpected_local_sources, "sapo.pt.csv")
  
  if (length(unexpected_local_sources) > 0) {
    warning(sprintf("Country '%s': %d local source(s) present in dropbox folder but not defined in constants.R:\n  %s", 
                   country, length(unexpected_local_sources), paste(unexpected_local_sources, collapse = ", ")), 
           call. = FALSE)
  }
  
  df_list <- lapply(csv_files, read_counts_csv)
  df <- do.call(rbind, df_list)
  
  # Add country columns
  df <- cbind(country = country, df)
  attr(df, "used_sources") <- valid_sources
  attr(df, "not_used_sources") <- not_used_sources
  attr(df, "missing_sources") <- missing_local_sources
  attr(df, "unexpected_sources") <- unexpected_local_sources
  df
}


#' Resolve Date Subfolder
#'
#' Resolves the date subfolder name for a country on Dropbox, handling
#' the "latest", "all", or specific date folder selection.
#'
#' @param type Character string. Type of data, either "rai" or "civic".
#' @param country Character string. Name of the country.
#' @param date Character string. One of "latest", "all", or a specific folder name like "2022_1_12".
#' @return Character vector of subfolder names.
#' @details This function:
#' \itemize{
#'   \item{Lists available date folders for the country}
#'   \item{Filters to standard date format folders}
#'   \item{Sorts folders chronologically}
#'   \item{Returns latest, all, or specific folder as requested}
#' }
#' @examples
#' \dontrun{
#' # Get latest date folder for Nigeria civic data
#' db_resolve_date("civic", "Nigeria", "latest")
#' 
#' # Get all available date folders
#' db_resolve_date("civic", "Nigeria", "all")
#' 
#' # Get specific date folder
#' db_resolve_date("rai", "Ukraine", "2024_12_1")
#' }
#' @keywords internal

db_resolve_date <- function(type = c("rai", "civic"), country, date = "latest") {
  type <- match.arg(type)

  # pick the right base path
  base_path <- if (type == "rai") path_counts_rai(country) else path_counts_civic(country)
  folders   <- dir(base_path)

  # keep only YYYY_M_D style, drop unparseable
  folders <- folders[grepl("^[0-9_]+$", folders)]
  dates   <- as.Date(folders, format = "%Y_%m_%d")
  keep    <- !is.na(dates)
  folders <- folders[keep]
  dates   <- dates[keep]

  if (!length(folders)) {
    stop(sprintf("No date folders found for '%s' (%s) under: %s", country, type, base_path))
  }

  folders <- folders[order(dates)]

  if (identical(date, "latest")) {
    # newest folder in the *same* tree
    res <- tail(folders, 1)
  } else if (identical(date, "all")) {
    res <- folders
  } else {
    if (!date %in% folders) {
      stop(sprintf("Folder '%s' does not exist, found: '%s'",
                   date, paste0(folders, collapse = "', '")))
    }
    res <- date
  }

  res
}


#' Extract Civic Counts by Source
#'
#' Extracts civic event counts by news source for a specific country from
#' Dropbox, handling the complex folder structure and source validation.
#'
#' @param country Character string. Name of the country to extract data for.
#' @param date Character string. Date folder to use ("latest" or specific folder name).
#' @param quiet Logical. If TRUE, suppress progress messages.
#' @param use_region_filter Logical. If TRUE, apply regional filtering to sources.
#' @return Data frame with civic counts by source, returned invisibly.
#' @details This function:
#' \itemize{
#'   \item{Resolves the appropriate date folder}
#'   \item{Handles legacy folder structures (Civic_Related, Combined, Non_Civic_Related)}
#'   \item{Validates and combines sources}
#'   \item{Merges civic relevant and non-civic relevant data}
#'   \item{Writes output to datastore}
#' }
#' @examples
#' \dontrun{
#' # Extract civic counts for Nigeria
#' extract_civic_counts_by_source("Nigeria")
#' 
#' # Extract with regional filtering
#' extract_civic_counts_by_source("Ukraine", use_region_filter = TRUE)
#' 
#' # Extract from specific date folder
#' extract_civic_counts_by_source("Kenya", date = "2024_12_1")
#' }
#' @export
extract_civic_counts_by_source <- function(country, date = "latest", quiet = FALSE, use_region_filter = FALSE) {
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
                                        country, use_region_filter = use_region_filter)
    combined_not_used_sources <- attr(combined, "not_used_sources")
    # fix -999 name
    names(combined)[names(combined) %in% "-999"] = "cs_999"
    original_column_order = colnames(combined)
    combined <- combined[, setdiff(colnames(combined), cr_vars)]
    
    csr <- vet_and_combine_sources(file.path(date_folder_path, "Civic_Related"),
                                   country, use_region_filter = use_region_filter)
    csr_not_used_sources <- attr(csr, "not_used_sources")
    csr <- csr[, c("country", "source", "date", cr_vars)]
    
    ncr <- vet_and_combine_sources(file.path(date_folder_path, "Non_Civic_Related"),
                                   country, use_region_filter = use_region_filter)
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


#' Read Civic Counts by Source
#'
#' Reads the processed civic counts by source data from the local datastore.
#'
#' @param country Character string. Name of the country to read data for.
#' @return Tibble with civic counts by source for the specified country.
#' @details This function reads the CSV files generated by \code{\link{extract_civic_counts_by_source}}
#' from the local datastore directory.
#' @examples
#' \dontrun{
#' # Read civic counts by source for Nigeria
#' nigeria_data <- read_civic_by_source("Nigeria")
#' 
#' # Check the structure
#' str(nigeria_data)
#' head(nigeria_data)
#' }
#' @export
read_civic_by_source <- function(country) {
  path <- here("data", "0-civic-by-source", sprintf("%s.csv", country))
  res <- readr::read_csv(path, show_col_types = FALSE)
  # this removes the spec attribute
  tibble::as_tibble(res)
}


# From: aggregate-and-merge.R ---------------------------------------------


#' Create Country-Month Civic Counts
#'
#' Aggregates civic event counts by country and month, creating normalized
#' variables and filtering to valid date ranges. The processed data are
#' written to the datastore.
#'
#' @param country Character string. Name of the country to process.
#' @param quiet Logical. If TRUE, suppress diagnostic messages.
#' @return Data frame with aggregated civic counts, returned invisibly.
#' @details This function:
#' \itemize{
#'   \item{Reads civic counts by source}
#'   \item{Aggregates by country and date}
#'   \item{Filters to valid date range using \code{\link{country_last_month}}}
#'   \item{Creates normalized variables (counts/total_articles)}
#'   \item{Writes output to datastore}
#' }
#' @examples
#' \dontrun{
#' # Aggregate civic data for Nigeria
#' aggregate_and_merge("Nigeria")
#' 
#' # Aggregate with diagnostic messages
#' aggregate_and_merge("Ukraine", quiet = FALSE)
#' }
#' @export
#' @aliases read_raw_counts update_story_counts update_source_entries read_source_entries
aggregate_and_merge <- function(country, quiet = TRUE) {
  df <- read_civic_by_source(country)
  df$source <- NULL
  df <- df |>
    dplyr::group_by(country, date) |>
    dplyr::summarise_all(sum) |>
    dplyr::ungroup()

  new_cols <- c("country","date", civic, "cs_999", paste0("ncr_", cr_vars), "total_articles")
  df <- df[, new_cols]

  any_na <- !all(complete.cases(df))
  if (any_na) warning(sprintf("Merged data for '%s' has missing values.", country))
  if (any_na & !quiet) cat(sprintf("Merged data for '%s' has missing values\n", country))

  df <- df %>% dplyr::filter(date <= country_last_month(!!country))

  safe_div <- function(num, den) ifelse(is.na(den) | den == 0, 0, num / den)
  df[paste0(civic, "Norm")] <-
    as.data.frame(lapply(df[civic], function(x) safe_div(x, df$total_articles)))
  df[paste0(paste0("ncr_", cr_vars), "Norm")] <-
    as.data.frame(lapply(df[paste0("ncr_", cr_vars)], function(x) safe_div(x, df$total_articles)))

  if (any(is.na(df$total_articles)) || any(df$total_articles == 0, na.rm = TRUE)) {
    message(sprintf("Info: %d rows had total_articles==0/NA; normalized values set to 0.",
                    sum(is.na(df$total_articles) | df$total_articles == 0, na.rm = TRUE)))
  }

  dir.create(here("data","1-civic-aggregate"), recursive = TRUE, showWarnings = FALSE)
  out_path <- here("data","1-civic-aggregate", sprintf("%s.csv", country))
  readr::write_csv(df, out_path)

  invisible(df)   
}

#' @describeIn aggregate_and_merge Creates the source entries matrix showing temporal coverage patterns.
#' @param use_rai Logical. If TRUE (default), join civic and RAI data. If FALSE, use only civic data.
#' @return Source entries matrix saved to datastore and returned invisibly.
#' @details This function creates a binary matrix showing when each local news source
#' was active (published articles) across the full time series. The matrix is essential
#' for tracking source entry/exit patterns over time.
#' @examples
#' \dontrun{
#' # Create source entries matrix with both civic and RAI data
#' update_source_entries()
#' 
#' # Create with only civic data
#' update_source_entries(use_rai = FALSE)
#' }
#' @export
update_source_entries <- function(use_rai = TRUE) {
  # Load civic data
  files <- dir(here("data","0-civic-by-source"), full.names = TRUE)
  civic <- lapply(files, readr::read_csv, show_col_types = FALSE, progress = FALSE)
  civic <- dplyr::bind_rows(civic)
  
  if (use_rai) {
    # Load and join with RAI data
    files <- dir(here("data","0-rai-by-source-and-influencer"), full.names = TRUE)
    rai <- lapply(files, readr::read_csv, show_col_types = FALSE)
    rai <- dplyr::bind_rows(rai)
    
    rai <- rai[rai$influencer=="Combined", ]
    rai$influencer <- NULL
    
    raw <- dplyr::inner_join(civic, rai, by = c("country", "date", "source", "total_articles"))
  } else {
    # Use only civic data
    raw <- civic
  }
  
  # record the full date range here because I'm going to alter raw below
  full_date_range <- sort(unique(raw$date))
  
  # Remove international and regional sources from list of all sources, we're only
  # interested in local sources (keep regional sources that double as local)
  remove_sources <- c(ml4p.forecast::isources, ml4p.forecast::rsources[! ml4p.forecast::rsources %in%  c("balkaninsight.com.csv", "indiatimes.com.csv", "timesofindia.indiatimes.com.csv", "theeastafrican.co.ke.csv") ] )
  remove_sources <- gsub(".csv", "", remove_sources)
  lsources_all <- setdiff(unique(raw$source), remove_sources)
  raw <- raw[raw$source %in% lsources_all, ]
  
  # We create binary indicators for each source that turn-on
  # in all months where the source has volume > 0
  last0 <- raw |>
    dplyr::select(country, source, date, total_articles) |>
    dplyr::mutate(total =  dplyr::case_when(total_articles > 0 ~ 1, TRUE ~ 0) ) |>
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
#' Returns the complete whitelist of all usable news sources for a country,
#' including international, regional, and local sources.
#'
#' @param country Character string. Name of the country.
#' @param use_region_filter Logical. If TRUE, only include regional sources 
#'   relevant to the country's geographic region. If FALSE, include all regional
#'   sources (legacy behavior). Default is FALSE for backward compatibility.
#' @return Character vector of all valid source CSV filenames for the country.
#' @details This function combines:
#' \itemize{
#'   \item{International sources (\code{\link{isources}})}
#'   \item{Regional sources (\code{\link{rsources}} or filtered by region)}
#'   \item{Local sources (from \code{\link{local_source_select}})}
#' }
#' @examples
#' # Get all sources for Nigeria (legacy behavior)
#' whitelist_sources("Nigeria")
#' 
#' # Get sources with regional filtering
#' whitelist_sources("Nigeria", use_region_filter = TRUE)
#' 
#' # Compare the difference
#' all_sources <- whitelist_sources("Ukraine")
#' filtered_sources <- whitelist_sources("Ukraine", use_region_filter = TRUE)
#' setdiff(all_sources, filtered_sources)
#' @export
#' @aliases isources rsources local_source_select vet_sources region_sources
whitelist_sources <- function(country, use_region_filter = FALSE) {
  stopifnot(length(country)==1)
  
  # Get regional sources based on filtering preference
  regional_sources <- if (use_region_filter) {
    region_sources(country)
  } else {
    rsources
  }
  
  c(
    isources,
    regional_sources,
    local_source_select(country)$lsources
  )
}

#' @describeIn whitelist_sources Validates a list of sources against the whitelist.
#' @param sources Character vector. Source names to validate.
#' @param country Character string. Country name for validation context.
#' @param use_region_filter Logical. Passed to whitelist_sources(). Default FALSE.
#' @return Character vector containing only the valid sources from the input list.
#' @details This function takes a list of source names and returns only those
#' that are present in the whitelist for the given country.
#' @examples
#' # Validate a list of sources for Nigeria
#' test_sources <- c("bbc.com.csv", "invalid_source.csv", "nation.africa.csv")
#' valid_sources <- vet_sources(test_sources, "Nigeria")
#' 
#' # Check what was filtered out
#' setdiff(test_sources, valid_sources)
#' @export
vet_sources <- function(sources, country, use_region_filter = FALSE) {
  valid <- whitelist_sources(country, use_region_filter = use_region_filter)
  valid_sources <- intersect(sources, valid)
  valid_sources
}



# RAI ---------------------------------------------------------------------

#' @describeIn path Path to the RAI counts folder
#' @param \dots Additional path components to append using [file.path()].
#' @return Character string representing the full path to the RAI counts folder or subdirectory.
#' @details This function constructs paths to the Dropbox folder containing
#' Regional and International (RAI) influence counts data. It's a convenience
#' wrapper around \code{path_dropbox}.
#' @examples
#' # Get path to RAI counts root
#' path_counts_rai()
#' 
#' # Get path to specific country
#' path_counts_rai("Ukraine")
#' 
#' # Get path to specific influencer folder
#' path_counts_rai("Ukraine", "2024_12_1", "Russia")
#' @export
path_counts_rai <- function(...) {
  path_dropbox("Counts_RAI_New", ...)
}

#' Read RAI Counts by Source and Influencer
#'
#' Reads the processed RAI counts by source and influencer data from the
#' local datastore.
#'
#' @param country Character string. Name of the country to read data for.
#' @return Tibble with RAI counts by source and influencer for the specified country.
#' @details This function reads the CSV files generated by
#' \code{\link{extract_rai_counts_by_source_and_influencer}} from the local
#' datastore directory. The data includes counts for Combined, China, and Russia
#' influencer categories.
#' @examples
#' \dontrun{
#' # Read RAI counts for Ukraine
#' ukraine_rai <- read_rai_by_source_and_influencer("Ukraine")
#' 
#' # Check the structure
#' str(ukraine_rai)
#' unique(ukraine_rai$influencer)
#' }
#' @export
read_rai_by_source_and_influencer <- function(country) {
  path <- here("data","0-rai-by-source-and-influencer", sprintf("%s.csv", country))
  res <- readr::read_csv(path, show_col_types = FALSE)
  # this removes teh spec attribute
  tibble::as_tibble(res)
}

#' Extract RAI Counts by Source and Influencer
#'
#' Extracts RAI (Regional and International) influence counts by source and
#' influencer for a country from Dropbox, combining separate CSV files into
#' a unified dataset.
#'
#' @param country Character string. Name of the country to extract data for.
#' @param date Character string. Date folder to use ("latest" or specific folder name).
#' @param quiet Logical. If TRUE, suppress progress messages.
#' @param use_region_filter Logical. If TRUE, apply regional filtering to sources.
#' @return Data frame with RAI counts by source and influencer, returned invisibly.
#' @details This function:
#' \itemize{
#'   \item{Processes data for three influencer categories: Combined, China, Russia}
#'   \item{Validates and combines sources for each influencer}
#'   \item{Merges with total article counts from civic data}
#'   \item{Writes output to datastore}
#' }
#' @examples
#' \dontrun{
#' # Extract RAI counts for Ukraine
#' extract_rai_counts_by_source_and_influencer("Ukraine")
#' 
#' # Extract with regional filtering
#' extract_rai_counts_by_source_and_influencer("Nigeria", use_region_filter = TRUE)
#' 
#' # Extract from specific date folder
#' extract_rai_counts_by_source_and_influencer("Belarus", date = "2024_12_1")
#' }
#' @export
#' @seealso [extract_article_total()], [extract_rai_counts_by_source_and_influencer()]

### Jeremy's original script- not merging counts with different rows
# extract_rai_counts_by_source_and_influencer <- function(country,
#                                                         date = "latest",
#                                                         quiet = TRUE,
#                                                         use_region_filter = FALSE) {
#   stopifnot(
#     "country must be just 1 country" = length(country)==1
#   )
  
#   date_folder <- db_resolve_date("rai", country, date)
#   if (!quiet) {
#     cat(paste0("Using '", date_folder, "/' folder\n"))
#   }
  
#   influencer_ops <- c("Combined", "China", "Russia")
  
#   # The date folder should contain Combined, Russia, China sub-folders
#   if (!all(influencer_ops %in% dir(path_counts_rai(country, date_folder)))) {
#     stop("could not find influencer folders")
#   }
  
#   df_by_influencer <- list()
#   for (influencer_i in influencer_ops) {
    
#     path <- path_counts_rai(country, date_folder, influencer_i)
    
#     df <- vet_and_combine_sources(path, country, use_region_filter = use_region_filter)
    
#     # fix -999 name
#     names(df)[names(df) %in% "-999"] = "rai_999"
    
#     # Add the influencer type and move to front
#     df$influencer <- influencer_i
#     front <- c("influencer")
#     df <- df[, c(front, setdiff(colnames(df), front))]
    
#     df_by_influencer[[influencer_i]] <- df
#   }
#   df_by_influencer <- do.call(rbind, df_by_influencer)
#   rownames(df_by_influencer) <- NULL
  
  
#   # Retrieve `total_articles` from civic counts
#   dft = read_csv(here("data", "0-civic-by-source", sprintf("%s.csv", country)), show_col_types = FALSE) %>%
#     dplyr::select(country, source, date, total_articles)
#   # RAI and civic should be the same length
#   stopifnot(
#     "rai and civic row numbers are not equal" = nrow(dft)==(nrow(df_by_influencer)/3)
#   )
  
#   df_by_influencer = left_join(df_by_influencer, dft)

#   # Write to datastore
#   outfile <- here("data", "0-rai-by-source-and-influencer", sprintf("%s.csv", country))
#   readr::write_csv(df_by_influencer, file = outfile, progress = FALSE)
  
#   invisible(df_by_influencer)
# }

### ZR's version with civic/rai padding
extract_rai_counts_by_source_and_influencer <- function(country,
                                                        date = "latest",
                                                        quiet = TRUE,
                                                        use_region_filter = FALSE) {
  #  Minimal fix:  pad civic rows to match the (country, source, date) grid that
  #  exists in the RAI data.  This removes the hard stop when one dataset starts
  #  earlier / ends later than the other after the repository migration.

  stopifnot("country must be just 1 country" = length(country) == 1)

  date_folder <- db_resolve_date("rai", country, date)
  if (!quiet) cat(sprintf("Using '%s/' folder\n", date_folder))

  influencer_ops <- c("Combined", "China", "Russia")

  # The date‑folder should contain Combined, Russia, China sub‑folders
  if (!all(influencer_ops %in% dir(path_counts_rai(country, date_folder)))) {
    stop("could not find influencer folders")
  }

  # 1. Build the (source, date) counts for each influencer
  df_by_influencer <- list()
  for (influencer_i in influencer_ops) {
    path <- path_counts_rai(country, date_folder, influencer_i)
    df   <- vet_and_combine_sources(path, country,
                                    use_region_filter = use_region_filter)

    # Fix "‑999" column name produced by vet_and_combine_sources()
    names(df)[names(df) %in% "-999"] <- "rai_999"

    # Tag with influencer and move that column to the front
    df$influencer <- influencer_i
    front <- c("influencer")
    df <- df[, c(front, setdiff(colnames(df), front))]

    df_by_influencer[[influencer_i]] <- df
  }
  df_by_influencer <- do.call(rbind, df_by_influencer)
  rownames(df_by_influencer) <- NULL

  # 2. Read civic counts once per (country, source, date)
  civic_raw <- readr::read_csv(
    here("data", "0-civic-by-source", sprintf("%s.csv", country)),
    show_col_types = FALSE
  ) %>%
    dplyr::select(country, source, date, total_articles)


  # 3. Pad civic counts so every (country, source, date) that appears in the rai
  #    grid exists in the civic grid (fill missing with 0).  This ensures the
  #    downstream join doesn't fail, regardless of differing start/end months.
  grid_keys <- df_by_influencer %>%
    dplyr::select(country, source, date) %>%
    dplyr::distinct()

  civic_aligned <- grid_keys %>%
    dplyr::left_join(civic_raw, by = c("country", "source", "date")) %>%
    dplyr::mutate(total_articles = tidyr::replace_na(total_articles, 0L))

  # a warning if, even after padding, we still see a mismatch.
  if (nrow(civic_aligned) != nrow(df_by_influencer) / 3) {
    warning("RAI and civic row numbers differ even after padding; please verify data integrity.")
  }

  # 4. Merge aligned civic counts back into the influencer‑expanded RAI frame
  df_by_influencer <- dplyr::left_join(
    df_by_influencer,
    civic_aligned,
    by = c("country", "source", "date")
  )

  # 5. Persist and return invisibly
  outfile <- here("data", "0-rai-by-source-and-influencer", sprintf("%s.csv", country))
  readr::write_csv(df_by_influencer, file = outfile, progress = FALSE)

  invisible(df_by_influencer)
}


#' Aggregate and Merge RAI Data
#'
#' Aggregates RAI (Regional and International) influence counts by country,
#' influencer, and month, creating normalized variables and theme calculations.
#' The processed data are written to separate files by influencer.
#'
#' @param country Character string. Name of the country to process.
#' @param quiet Logical. If TRUE, suppress diagnostic messages.
#' @return Data frame with aggregated RAI counts, returned invisibly.
#' @details This function:
#' \itemize{
#'   \item{Reads RAI counts by source and influencer}
#'   \item{Aggregates by influencer, country, and date}
#'   \item{Filters to valid date range using \code{\link{country_last_month}}}
#'   \item{Creates normalized variables (counts/total_articles)}
#'   \item{Calculates RAI theme sums based on \code{data/rai_vars.csv}}
#'   \item{Writes separate files for each influencer}
#' }
#' @examples
#' \dontrun{
#' # Aggregate RAI data for Ukraine
#' aggregate_and_merge_rai("Ukraine")
#' 
#' # Aggregate with diagnostic messages
#' aggregate_and_merge_rai("Belarus", quiet = FALSE)
#' }
#' @export
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
  # df[, paste0(rai , "Norm")]   <- as.data.frame(lapply(df[,rai], function(x) x/df[, "total_articles"]))
  safe_div <- function(num, den) ifelse(is.na(den) | den == 0, 0, num / den)

  df[ paste0(rai, "Norm") ] <-
    as.data.frame(lapply(df[ rai ], function(x) safe_div(x, df$total_articles)))
    
  # Add RAI theme calculations based on data/rai_vars.csv
  rai_vars <- readr::read_csv(here("data", "rai_vars.csv"), show_col_types = FALSE)
  
  # Get unique themes
  unique_themes <- unique(rai_vars$theme)
  
  # Calculate sum for each theme
  for (theme_code in unique_themes) {
    # Get variables for this theme
    theme_vars <- rai_vars$id[rai_vars$theme == theme_code]
    # Add "Norm" suffix to match column names
    theme_vars_norm <- paste0(theme_vars, "Norm")
    # Keep only variables that exist in the data
    theme_vars_avail <- theme_vars_norm[theme_vars_norm %in% names(df)]
    
    if (length(theme_vars_avail) > 0) {
      # Create theme sum column name
      theme_name <- unique(rai_vars$theme_name[rai_vars$theme == theme_code])[1]
      theme_col_name <- paste0(tolower(gsub(" ", "_", theme_name)), "_sumNorm")
      
      # Calculate sum
      df[[theme_col_name]] <- rowSums(df[, theme_vars_avail, drop = FALSE])
      
      if (!quiet) {
        cat(sprintf("Added theme '%s' with %d variables\n", theme_name, length(theme_vars_avail)))
      }
    }
  }
  
  # Write separate CSV files for each influencer
  for (influencer_name in unique(df$influencer)) {
    influencer_data <- df[df$influencer == influencer_name, ]

    # Create filename: country_influencer.csv (e.g., Belarus_russia.csv)
    filename <- sprintf("%s_%s.csv", country, tolower(influencer_name))
    out_path <- here("data", "1-rai-aggregate", filename)
    readr::write_csv(influencer_data, out_path)
    
    if (!quiet) {
      cat(sprintf("Written RAI data for %s-%s to %s\n", country, influencer_name, filename))
    }
  }
  
  invisible(df)
}
