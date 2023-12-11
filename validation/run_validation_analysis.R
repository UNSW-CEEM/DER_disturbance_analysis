# this script will run the tool based on the settings files in validation/data/...
# results are output to CSV based on the given settings files

# MANUALLY SET HERE IF USING RSTUDIO
OUTPUT_PREFIX <- "ref" # "test"

# Handle command line calls to overwrite prefix
if (!interactive()) {
  args <- commandArgs(TRUE)
  if (!is.na(args[1])) {
    OUTPUT_PREFIX <- args[1]
  }
}

logging::basicConfig()

base_directory_name <- basename(getwd())
if (base_directory_name == "DER_disturbance_analysis") {
  tool_directory <- getwd()
} else {
  stop("Script is not being run in DER_disturbance_analysis folder, make sure that tool directory ahs been set")
}
source(sprintf("%s/load_tool_environment.R", tool_directory))

load_settings <- function(settings_file) {
  settings <- c()
  tryCatch(
    {
      settings <- fromJSON(file = settings_file)
    },
    error = function(cond) {
      logging::logerror("Something went wrong loading the settings, please see the console for more details.")
    }
  )
  return(settings)
}

create_blank_data <- function() {
  data <- list(
    combined_data = data.frame(),
    combined_data_no_ac_filter = data.frame(),
    agg_power = data.frame(),
    agg_norm_power = data.frame(),
    install_data = data.frame(),
    site_details = data.frame(),
    circuit_details = data.frame(),
    circuit_details_for_editing = data.frame(),
    site_details_raw = data.frame(),
    site_details_for_editing = data.frame(),
    proxy_site_details_editor = 1,
    proxy_circuit_details_editor = 1,
    combined_data_after_clean = data.frame(),
    time_series_data = data.frame(),
    sample_count_table = data.frame(),
    combined_data_f = data.frame(),
    performance_factors = data.frame(),
    postcode_data = data.frame(),
    response_count = data.frame(),
    zone_count = data.frame(),
    distance_response = data.frame(),
    frequency_data = data.frame(),
    unique_offsets = c(),
    circuit_summary = data.frame(),
    region_frequency = data.frame(),
    trigger_update_manual_compliance_tab = FALSE
  )
  return(data)
}

run_analysis_from_settings <- function(settings) {
  data <- create_blank_data()

  # connect to DB
  data$db <- DBInterface$new()
  data$db$connect_to_existing_database(settings$database_name)

  # load data
  load_results <- load_data(data, settings)
  data <- load_results$data
  load_errors <- load_results$errors
  rm(load_results)

  # show errors and warnings on tool dash
  if (length(load_errors$warnings) > 0) {
    for (warning in load_errors$warnings) {
      logging::logwarn(paste(warning$title, warning$body), logger = app_logger)
    }
  }
  # do not proceed if errors have been raised
  if (length(load_errors$errors) > 0) {
    for (error in load_errors$errors) {
      logging::logerror(paste(warning$title, warning$body), logger = app_logger)
      stop("errors in data loading, see logs")
    }
  }

  settings$pre_event_interval <- as.POSIXct(settings$pre_event_interval, tz = "Australia/Brisbane")

  # run analysis
  analysis_results <- run_analysis(data, settings)

  # show errors and warnings on tool dash
  if (length(analysis_results$errors$warnings) > 0) {
    for (warning in load_errors$warnings) {
        logging::logwarn(paste(warning$title, warning$body), logger = app_logger)
    }
  }
  # do not proceed if errors have been raised
  if (length(analysis_results$errors$errors) > 0) {
    for (error in load_errors$errors) {
      logging::logerror(paste(warning$title, warning$body), logger = app_logger)
      stop("errors in analysis, see logs")
    }
  }
  return(analysis_results)
}

data_dirs <- list.dirs('validation/data', recursive = FALSE)

# iterate across all validation data folders
if (length(data_dirs) > 0) {
  for (dir in data_dirs) {
    all_files_in_dir <- list.files(dir)
    settings_fname <- sprintf("%s/%s_meta_data.json", dir, OUTPUT_PREFIX)
    circuit_summary_fname <- sprintf("%s/%s_circ_sum.csv", dir, OUTPUT_PREFIX)
    underlying_data_fname <- sprintf("%s/%s_underlying.csv", dir, OUTPUT_PREFIX)
    settings <- load_settings(settings_fname)
    results <- run_analysis_from_settings(settings)
    write.csv(results$data$circuit_summary, circuit_summary_fname, row.names = FALSE)
    write.csv(results$data$combined_data_f, underlying_data_fname, row.names = FALSE)
  }
} else {
  logging::logerror("No data found in directory")
}
