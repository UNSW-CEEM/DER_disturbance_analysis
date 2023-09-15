# Do not run this file directly, run testthat.R

testthat::context("Testing the creation of a database from csv files.")
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

load_test_file <- function(path_name) {
  data <- read.csv(file = path_name, header = TRUE, stringsAsFactors = FALSE)
  return(data)
}


testthat::test_that("Building works when there are different duration values with the same utcstamp", {
  timeseries_path_name <- "data/simple_timeseries_different_durations_same_utcstamp.csv"
  site_details_path_name <- "data/simple_site_details.csv"
  circuit_details_path_name <- "data/simple_circuit_details.csv"

  # We expect the output of loading via the database to be the same as if we just read straight from CSV.
  expected_timeseries <- load_test_file(timeseries_path_name)
  expected_site_details <- load_test_file(site_details_path_name)
  expected_circuit_details <- load_test_file(circuit_details_path_name)
  expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
  expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')

  # Create the DBInterface and test creating the database.
  if (file.exists("test.db")) {
    file.remove("test.db")
  }
  dp <- DBInterface$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(
    timeseries = timeseries_path_name,
    circuit_details = circuit_details_path_name,
    site_details = site_details_path_name,
    check_dataset_ids_match = FALSE
  )

  output_timeseries <- dp$get_time_series_data()
  output_site_details <- dp$get_site_details_raw()
  output_circuit_details <- dp$get_circuit_details_raw()

  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})


testthat::test_that("Building works when there are null duration values", {
  timeseries_path_name <- "data/timeseries_null_duration.csv"
  site_details_path_name <- "data/simple_site_details.csv"
  circuit_details_path_name <- "data/simple_circuit_details.csv"

  # We expect the output of loading via the database to be the same as if we just read straight from CSV.
  expected_timeseries <- load_test_file("data/timeseries_null_duration_cleaned.csv")
  expected_site_details <- load_test_file(site_details_path_name)
  expected_circuit_details <- load_test_file(circuit_details_path_name)
  expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
  expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')

  # Create the DBInterface and test creating the database.
  if (file.exists("test.db")) {
    file.remove("test.db")
  }
  dp <- DBInterface$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(
    timeseries = timeseries_path_name,
    circuit_details = circuit_details_path_name,
    site_details = site_details_path_name,
    check_dataset_ids_match = FALSE
  )

  dp$add_postcode_lon_lat_to_database("data/postcode_lon_lat.csv")
  dp$add_manufacturer_mapping_table("data/manufacturer_mapping.csv")
  dp$run_data_cleaning_loop(max_chunk_size = 1)

  output_timeseries <- dp$get_time_series_data()
  output_site_details <- dp$get_site_details_raw()
  output_circuit_details <- dp$get_circuit_details_raw()
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})
