library(lubridate)

# Do not run this file directly, run testthat.R

testthat::context("Testing the creation of a database from csv files.")
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

load_test_file <- function(path_name){
  data <- read.csv(file = path_name, header = TRUE, stringsAsFactors = FALSE)
  return(data)
}


#Setup database for testing getters.

timeseries_path_name <- "data/simple_timeseries.csv"
site_details_path_name <- "data/simple_site_details.csv"
circuit_details_path_name <- "data/simple_circuit_details.csv"

# We expect the output of loading via the database to be the same as if we
# just read straight from csv.
expected_timeseries <- load_test_file(timeseries_path_name)
expected_site_details <- load_test_file(site_details_path_name)
expected_circuit_details <- load_test_file(circuit_details_path_name)

# Create the DBInterface and test creating the database.
if (file.exists("test.db")) {file.remove("test.db")}
dp <- DBInterface$new()
dp$connect_to_new_database("test.db")
dp$build_database(timeseries = timeseries_path_name,
                  circuit_details = circuit_details_path_name,
                  site_details = site_details_path_name,
                  check_dataset_ids_match = FALSE)

dp$add_postcode_lon_lat_to_database("data/postcode_lon_lat.csv")
dp$add_manufacturer_mapping_table("data/manufacturer_mapping.csv")
dp$run_data_cleaning_loop(max_chunk_size = 10)


testthat::test_that("Test get_min_timestamp",{
  min_timestamp <- dp$get_min_timestamp()
  expected_time <- fastPOSIXct('2018-01-01 00:00:05', tz = "Australia/Brisbane")
  testthat::expect_equal(min_timestamp, expected_time)
})

testthat::test_that("Test get_max_timestamp",{
  max_timestamp <- dp$get_max_timestamp()
  expected_time <- fastPOSIXct('2018-01-01 00:00:40',tz = "Australia/Brisbane")
  testthat::expect_equal(max_timestamp, expected_time)
})

testthat::test_that("Test get_max_circuit_powers simple case",{
  max_powers <- dp$get_max_circuit_powers('NSW')
  expected_results <- data.frame(c_id = c(1, 1),
                                 max_power = c((100/5)/1000,
                                               (100/5)/1000),
                                 clean = c('raw', 'clean'),
                                 stringsAsFactors = FALSE)
  testthat::expect_equal(max_powers, expected_results)
})

testthat::test_that("Test check_if_table_exists works",{
  flag <- dp$check_if_table_exists('timeseries')
  testthat::expect_equal(flag, TRUE)
  flag <- dp$check_if_table_exists('blah')
  testthat::expect_equal(flag, FALSE)
})

#Setup database for testing getters.

timeseries_path_name <- "data/timeseries_max_power_testing.csv"
site_details_path_name <- "data/site_details_max_power_testing.csv"
circuit_details_path_name <- "data/circuit_details_max_power_testing.csv"

# We expect the output of loading via the database to be the same as if we
# just read straight from csv.
expected_timeseries <- load_test_file(timeseries_path_name)
expected_site_details <- load_test_file(site_details_path_name)
expected_circuit_details <- load_test_file(circuit_details_path_name)

# Create the DBInterface and test creating the database.
if (file.exists("test.db")) {file.remove("test.db")}
dp <- DBInterface$new()
dp$connect_to_new_database("test.db")
dp$build_database(timeseries = timeseries_path_name,
                  circuit_details = circuit_details_path_name,
                  site_details = site_details_path_name,
                  check_dataset_ids_match = FALSE)

dp$add_postcode_lon_lat_to_database("data/postcode_lon_lat.csv")
dp$add_manufacturer_mapping_table("data/manufacturer_mapping.csv")
dp$run_data_cleaning_loop(max_chunk_size = 10)

testthat::test_that("Test get_max_circuit_powers complex case",{
  max_powers <- dp$get_max_circuit_powers('NSW')
  expected_results <- data.frame(c_id = c(1, 2, 3, 1, 2, 3),
                                 max_power = c((100/5)/1000,
                                               (10000/60)/1000,
                                               (-101/5)/1000,
                                               (100/5)/1000,
                                               (10000/60)/1000,
                                               (-101/5)/1000),
                                 clean = c('raw', 'raw', 'raw',
                                           'clean', 'clean', 'clean'),
                                 stringsAsFactors = FALSE)
  testthat::expect_equal(max_powers, expected_results)
})

