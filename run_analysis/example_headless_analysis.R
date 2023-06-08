source("load_tool_environment.R")

SETTINGS_FILE <- "validation/data/tesla/ref_meta_data.json"
CIRCUIT_SUMMARY_OUTPUT <- "validation/data/tesla/test_circ_sum.csv"
UNDERLYING_DATA_OUTPUT <- "validation/data/tesla/test_underlying.csv"
app_logger <- "example_logger"
logging::addHandler(writeToFile, logger=app_logger, file="logging/headless_logs.log")

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

# read settings
settings <- load_settings(SETTINGS_FILE)

# connect to DB
data$db <- DBInterface$new()
data$db$connect_to_existing_database(settings$database_name)

# load data
load_results <- load_data(data, settings)
data <- load_results$data
load_errors <- load_results$errors
rm(load_results)

# show errors & warnings on tool dash
if (length(load_errors$warnings) > 0) {
    for (warning in load_errors$warnings) {
        logging::logwarn(paste(warning$title, warning$body), logger=app_logger)
    }
}
# do not proceed if errors have been raised
if (length(load_errors$errors) > 0) {
    for (error in load_errors$errors) {
        logging::logerror(paste(warning$title, warning$body), logger=app_logger)
        stop("errors in data loading, see logs")
    }
}

settings$pre_event_interval <- as.POSIXct(settings$pre_event_interval, tz = "Australia/Brisbane")

# run analysis
analysis_results <- run_analysis(data, settings)
data <- analysis_results$data
analysis_errors <- analysis_results$errors
rm(analysis_results)

write.csv(data$circuit_summary, CIRCUIT_SUMMARY_OUTPUT, row.names=FALSE)
write.csv(data$combined_data_f, UNDERLYING_DATA_OUTPUT, row.names=FALSE)
