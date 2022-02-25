if (!exists("app_logger")) {
    logging::basicConfig()
    logger <- getLogger()$name
} else {
    logger <- app_logger
}
 
INSTALL_DATA_FILE <- "inbuilt_data/cer_cumulative_capacity_and_number.csv"
CER_MANUFACTURER_DATA <- "inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv"
OFF_GRID_POSTCODES <- "inbuilt_data/off_grid_postcodes.csv"
POSTCODE_DATA_FILE <- "inbuilt_data/PostcodesLatLongQGIS.csv"

#' Validate csv inputs
#' 
validate_load_times <- function(settings, errors) {
  if (missing(errors)) {
    errors <- list(warnings=list(), errors=list())
  }
  if (is.na(as.character(settings$load_start_time)) | is.na(as.character(settings$load_end_time))) {
    errors$errors[[length(errors$errors) + 1]] <- list(title="Please provide a valid start and end date.", body='')
  } else {
    start_time <- format(as.POSIXct(settings$load_start_time, tz = "Australia/Brisbane"), tz = 'GMT')
    end_time <- format(as.POSIXct(settings$load_end_time, tz = "Australia/Brisbane"), tz = 'GMT')
    if (difftime(end_time, start_time, units = 'hours') > 2) {
      long_error_message <- c("A time window of greater than 2 hours was provided, it might take a while to load.")
      long_error_message <- paste(long_error_message, collapse = '')
      errors$warnings[[length(errors$warnings) + 1]] <- list(title="Warning long time window", body=long_error_message)
    }
    if (!(settings$load_end_time > settings$load_start_time)) {
      long_error_message <- c("The start time must be before the end time.")
      long_error_message <- paste(long_error_message, collapse = '')
      errors$errors[[length(errors$errors) + 1]] <- list(title="Error in time window", body=long_error_message)
    }
  }
  return(errors)
}

validate_required_files <- function(errors) {
  if (missing(errors)) {
    errors <- list(warnings=list(), errors=list())
  }
  if (!file.exists(INSTALL_DATA_FILE)){
    long_error_message <- c("The required file cer_cumulative_capacity_and_number.csv could ",
                            "not be found. Please add it to the inbuilt_data directory.")
    long_error_message <- paste(long_error_message, collapse = '')
    errors$errors[[length(errors$errors) + 1]] <- list(title="Error loading install data", body=long_error_message)
  }
  if (!file.exists(CER_MANUFACTURER_DATA)){
    long_error_message <- c("The required file cer_manufacturer_data could ",
                            "not be found. Please add it to the inbuilt_data directory.")
    long_error_message <- paste(long_error_message, collapse = '')
    errors$errors[[length(errors$errors) + 1]] <- list(title="Error loading manufacturer install data", body=long_error_message)
  }
  if (!file.exists(OFF_GRID_POSTCODES)){
    long_error_message <- c("The required file off_grid_postcodes could ",
                            "not be found. Please add it to the inbuilt_data directory.")
    long_error_message <- paste(long_error_message, collapse = '')
    errors$errors[[length(errors$errors) + 1]] <- list(title="Error loading off grid post code data", body=long_error_message)
  }
  return(errors)
}

validate_frequency_data <- function(settings, errors) {
  if (missing(errors)) {
    errors <- list(warnings=list(), errors=list())
  }
  if(settings$frequency_data_file != '') {
    frequency_data <- read.csv(file=settings$frequency_data_file, header=TRUE, stringsAsFactors = FALSE)
    expected_columns_names <- c("ts", "QLD", "NSW", "VIC", "SA", "TAS", "WA")
    column_names <- names(frequency_data)
    for (col_name in column_names){ 
      if (!(col_name %in% expected_columns_names)){
        long_error_message <- c("The frequency data csv should only contain the following columns, ", 
                                "ts, QLD, NSW, VIC, SA, TAS, WA. If this error persists after checking ",
                                "the columns names, then the file may be incorrectly incoded, please ", 
                                "re-save it using the CSV (Comma delimited) (*.csv) option in excel.")
        long_error_message <- paste(long_error_message, collapse = '')
        errors$errors[[length(errors$errors) + 1]] <- list(title="Error loading frequency data", body=long_error_message)
      }
    }
    if (!(settings$region_to_load %in% column_names)){
      long_error_message <- c("The frequency data csv must contain a column for the region being loaded. ", 
                              "Either choose to load no frequency data or ensure it containts data for the ",
                              "selected region.")
      long_error_message <- paste(long_error_message, collapse = '')
      errors$errors[[length(errors$errors) + 1]] <- list(title="Error loading frequency data", body=long_error_message)
    }
  }
  return(errors)
}

validate_timeseries_data <- function(time_series_data, errors) {
  if (missing(errors)) {
    errors <- list(warnings=list(), errors=list())
  }
  if (dim(time_series_data)[1] == 0){
    long_error_message <- c("The region and duration selected resulted in an empty dataset, please try another selection")
    long_error_message <- paste(long_error_message, collapse = '')
    errors$errors[[length(errors$errors) + 1]] <- list(title="Error loading timeseries data", body=long_error_message)
  }
  return(errors)
}

#' Load input from csvs specified in settings
#' @returns data object updated to included data from csvs if successful, else errors
load_data <- function(data, settings) {
  error_check_passed <- TRUE

  # Perform error checking before loading data.
  errors <- validate_load_times(settings)
  errors <- validate_required_files(errors)
  errors <- validate_frequency_data(settings, errors)

  # Check timerseries for errors
  start_time <- format(as.POSIXct(settings$load_start_time, tz = "Australia/Brisbane"), tz = 'GMT')
  end_time <- format(as.POSIXct(settings$load_end_time, tz = "Australia/Brisbane"), tz = 'GMT')
  time_series_data <- data$db$get_filtered_time_series_data(state = settings$region_to_load, duration = settings$duration, 
                                                          start_time = start_time, end_time = end_time)
  errors <- validate_timeseries_data(time_series_data, errors)

  # -------- data loading --------
  if (length(errors$errors) == 0) {
    site_details_raw <- data$db$get_site_details_raw()
    site_details_raw <- process_raw_site_details(site_details_raw)
    data$site_details <- mutate(site_details_raw, clean = 'raw')
    
    if (data$db$check_if_table_exists('site_details_cleaned')){
      site_details_clean <- data$db$get_site_details_cleaned()
      site_details_clean <- process_raw_site_details(site_details_clean)
      site_details_clean <- mutate(site_details_clean, clean = 'clean')
      data$site_details <- bind_rows(data$site_details, site_details_clean)
    }
    
    data$site_details <- site_categorisation(data$site_details)
    data$site_details <- size_grouping(data$site_details)

    circuit_details_raw <- data$db$get_circuit_details_raw()
    data$circuit_details <- mutate(circuit_details_raw, clean = 'raw')
    data$circuit_details_raw <- data$circuit_details
    
    if (data$db$check_if_table_exists('circuit_details_cleaned')){
      circuit_details_clean <- data$db$get_circuit_details_cleaned()
      circuit_details_clean <- mutate(circuit_details_clean, clean = 'clean')
      data$circuit_details <- bind_rows(data$circuit_details, circuit_details_clean)
    }

    time_series_data <- process_time_series_data(time_series_data)

    data$combined_data <- inner_join(time_series_data, data$circuit_details, by = "c_id")
    data$combined_data <- inner_join(data$combined_data, data$site_details, by = c("site_id", "clean"))
    
    data$combined_data <- perform_power_calculations(data$combined_data)

    # Load in the install data from CSV.
    install_data <- read.csv(file = INSTALL_DATA_FILE, header = TRUE, stringsAsFactors = FALSE)
    data$install_data <- process_install_data(install_data)
    
    manufacturer_install_data <- read.csv(file = CER_MANUFACTURER_DATA, header = TRUE, stringsAsFactors = FALSE)
    data$manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)
    
    postcode_data <- read.csv(file=POSTCODE_DATA_FILE, header=TRUE, stringsAsFactors = FALSE)
    data$postcode_data <- process_postcode_data(postcode_data)
    
    data$off_grid_postcodes <- read.csv(file=OFF_GRID_POSTCODES, header=TRUE, stringsAsFactors = FALSE)
    data$off_grid_postcodes <- data$off_grid_postcodes$postcodes
    
    if(settings$frequency_data_file != ''){
      data$frequency_data <- read.csv(file=settings$frequency_data_file, header=TRUE, stringsAsFactors = FALSE)
      data$frequency_data <- mutate(data$frequency_data, 
                                  ts=as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")))
    } else {
      data$frequency_data <- data.frame()
    }
    
    # Get offset filter options and label
    data$combined_data <- get_time_offsets(data$combined_data)
    data$unique_offsets <- get_time_series_unique_offsets(data$combined_data)
  }
  results <- list()
  results$data <- data
  results$errors <- errors
  return(results)
}
