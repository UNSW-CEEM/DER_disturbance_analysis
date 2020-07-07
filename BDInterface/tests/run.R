setwd(dirname(parent.frame(2)$ofile))
source("../R/interface.R")


load_test_file <- function(path_name){
  data <- read.csv(file = path_name, header = TRUE, stringsAsFactors = FALSE)
  return(data)
}

timeseries <- "testthat/data/smoke_timeseries.csv"
site_details_path_name <- "testthat/data/smoke_site_details.csv"
circuit_details_path_name <- "testthat/data/smoke_circuit_details.csv"
circuit_details_path_name_clean <- "testthat/data/smoke_circuit_details_cleaned.csv"

expected_timeseries <- load_test_file(timeseries)
expected_site_details <- load_test_file(site_details_path_name)
expected_circuit_details <- load_test_file(circuit_details_path_name)
expected_circuit_details <- mutate(expected_circuit_details, manual_compliance = 'Not set')
expected_circuit_details_cleaned <- load_test_file(circuit_details_path_name_clean)
expected_circuit_details_cleaned <- mutate(expected_circuit_details_cleaned, manual_compliance = 'Not set')

# Create the DataProcessor and test creating the database.
if (file.exists("test.db")) {file.remove("test.db")}
dp <- DataProcessor$new()
dp$connect_to_new_database("test.db")
dp$build_database(timeseries = timeseries,
                  circuit_details = circuit_details_path_name,
                  site_details = site_details_path_name)
dp$add_postcode_lon_lat_to_database("testthat/data/postcode_lon_lat.csv")

dp$run_data_cleaning_loop(max_chunk_size = 2)

output_timeseries <- dp$get_time_series_data()
output_site_details <- dp$get_site_details_raw()
output_circuit_details <- dp$get_circuit_details_raw()
output_circuit_details_cleaned <- dp$get_circuit_details_cleaning_report()

browser()
