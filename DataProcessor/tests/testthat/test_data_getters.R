# Do not run this file directly, run testthat.R

testthat::context("Testing the creation of a database from csv files.")


load_test_file <- function(path_name){
  data <- read.csv(file = path_name, header = TRUE, stringsAsFactors = FALSE)
  return(data)
}


#Setup database for testing getters.

timeseries_path_name <- "data/smoke_timeseries.csv"
site_details_path_name <- "data/smoke_site_details.csv"
circuit_details_path_name <- "data/smoke_circuit_details.csv"

# We expect the output of loading via the database to be the same as if we
# just read straight from csv.
expected_timeseries <- load_test_file(timeseries_path_name)
expected_site_details <- load_test_file(site_details_path_name)
expected_circuit_details <- load_test_file(circuit_details_path_name)

# Create the DataProcessor and test creating the database.
if (file.exists("test.db")) {file.remove("test.db")}
dp <- DataProcessor$new()
dp$connect_to_new_database("test.db")
dp$build_database(timeseries = timeseries_path_name,
                  circuit_details = circuit_details_path_name,
                  site_details = site_details_path_name)


testthat::test_that("Test get_min_timestamp",{
  min_timestamp <- dp$get_min_timestamp()
  testthat::expect_equal(min_timestamp, '2018-01-01 00:00:05')
})

testthat::test_that("Test get_max_timestamp",{
  max_timestamp <- dp$get_max_timestamp()
  testthat::expect_equal(max_timestamp, '2018-01-01 00:00:40')
})