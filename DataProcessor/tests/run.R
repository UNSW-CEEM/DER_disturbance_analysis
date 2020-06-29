setwd(dirname(parent.frame(2)$ofile))
source("../R/interface.R")


load_test_file <- function(path_name){
  data <- read.csv(file = path_name, header = TRUE, stringsAsFactors = FALSE)
  return(data)
}

#Setup database for testing getters.

timeseries_path_name <- "testthat/data/smoke_timeseries.csv"
site_details_path_name <- "testthat/data/smoke_site_details.csv"
site_details_processed_path_name <- "testthat/data/smoke_site_details_processed.csv"
circuit_details_path_name <- "testthat/data/smoke_circuit_details.csv"

expected_timeseries <- load_test_file(timeseries_path_name)
expected_site_details <- load_test_file(site_details_processed_path_name)
expected_circuit_details <- load_test_file(circuit_details_path_name)

# Create the DataProcessor and test creating the database.
if (file.exists("test.db")) {file.remove("test.db")}
dp <- DataProcessor$new()
dp$connect_to_new_database("test.db")
dp$build_database(timeseries = timeseries_path_name,
                  circuit_details = circuit_details_path_name,
                  site_details = site_details_path_name)
dp$create_processed_copy_of_site_details()

output_timeseries <- dp$get_time_series_data()
output_site_details <- dp$get_site_details_processed()
output_circuit_details <- dp$get_circuit_details_raw()

browser()
