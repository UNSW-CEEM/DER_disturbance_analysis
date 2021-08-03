# Do not run this file directly, run testthat.R

testthat::context("Testing the creation of a database from csv files.")


load_test_file <- function(path_name){
  data <- read.csv(file = path_name, header = TRUE, stringsAsFactors = FALSE)
  return(data)
}


testthat::test_that("Building works when no column aliases are required.",{

  timeseries_path_name <- "data/simple_timeseries.csv"
  site_details_path_name <- "data/simple_site_details.csv"
  circuit_details_path_name <- "data/simple_circuit_details.csv"

  # We expect the output of loading via the database to be the same as if we
  # just read straight from csv.
  expected_timeseries <- load_test_file(timeseries_path_name)
  expected_site_details <- load_test_file(site_details_path_name)
  expected_circuit_details <- load_test_file(circuit_details_path_name)
  expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
  expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')

  # Create the DBInterface and test creating the database.
  if (file.exists("test.db")) {file.remove("test.db")}
  dp <- DBInterface$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries = timeseries_path_name,
                    circuit_details = circuit_details_path_name,
                    site_details = site_details_path_name,
                    check_dataset_ids_match=FALSE)

  output_timeseries <- dp$get_time_series_data()
  output_site_details <- dp$get_site_details_raw()
  output_circuit_details <- dp$get_circuit_details_raw()

  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})

testthat::test_that("Building works when there are duplicates in ts data.",{

  timeseries_duplicates_path_name <- "data/timeseries_with_duplicates.csv"
  timeseries_no_duplicates_path_name <- "data/simple_timeseries.csv"
  site_details_path_name <- "data/simple_site_details.csv"
  circuit_details_path_name <- "data/simple_circuit_details.csv"

  expected_timeseries <- load_test_file(timeseries_no_duplicates_path_name)
  expected_site_details <- load_test_file(site_details_path_name)
  expected_circuit_details <- load_test_file(circuit_details_path_name)
  expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
  expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')

  # Create the DBInterface and test creating the database.
  if (file.exists("test.db")) {file.remove("test.db")}
  dp <- DBInterface$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries = timeseries_duplicates_path_name,
                    circuit_details = circuit_details_path_name,
                    site_details = site_details_path_name,
                    check_dataset_ids_match=FALSE)

  output_timeseries <- dp$get_time_series_data()
  output_site_details <- dp$get_site_details_raw()
  output_circuit_details <- dp$get_circuit_details_raw()

  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})

testthat::test_that("Building works when there are extra headers in the ts data.",{

  timeseries_with_extra_header <- "data/timeseries_with_extra_header.csv"
  timeseries_no_extra_header <- "data/simple_timeseries.csv"
  site_details_path_name <- "data/simple_site_details.csv"
  circuit_details_path_name <- "data/simple_circuit_details.csv"

  expected_timeseries <- load_test_file(timeseries_no_extra_header)
  expected_site_details <- load_test_file(site_details_path_name)
  expected_circuit_details <- load_test_file(circuit_details_path_name)
  expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
  expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')

  # Create the DBInterface and test creating the database.
  if (file.exists("test.db")) {file.remove("test.db")}
  dp <- DBInterface$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries = timeseries_with_extra_header,
                    circuit_details = circuit_details_path_name,
                    site_details = site_details_path_name,
                    check_dataset_ids_match=FALSE)

  dp$drop_repeated_headers()

  output_timeseries <- dp$get_time_series_data()
  output_site_details <- dp$get_site_details_raw()
  output_circuit_details <- dp$get_circuit_details_raw()

  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})

testthat::test_that("test data cleaning works, with batch size
                    smaller than number of circuits.",{
                      
  timeseries_with_missing_durations <- "data/timeseries_with_missing_durations.csv"
  timeseries_expected_out <- "data/simple_timeseries.csv"
  site_details_path_name <- "data/simple_site_details.csv"
  site_details_cleaned_path_name <- "data/simple_site_details_cleaned.csv"
  circuit_details_path_name <- "data/simple_circuit_details.csv"

  expected_timeseries <- load_test_file(timeseries_expected_out)
  expected_site_details <- load_test_file(site_details_path_name)
  expected_site_details_cleaned <- load_test_file(site_details_cleaned_path_name)
  expected_circuit_details <- load_test_file(circuit_details_path_name)
  expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
  expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')
  
  # Create the DBInterface and test creating the database.
  if (file.exists("test.db")) {file.remove("test.db")}
  dp <- DBInterface$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries = timeseries_with_missing_durations,
                    circuit_details = circuit_details_path_name,
                    site_details = site_details_path_name,
                    check_dataset_ids_match=FALSE)
  
  dp$add_postcode_lon_lat_to_database("data/postcode_lon_lat.csv")
  dp$add_manufacturer_mapping_table("data/manufacturer_mapping.csv")
  dp$run_data_cleaning_loop(max_chunk_size = 1)
  
  output_timeseries <- dp$get_time_series_data()
  output_site_details <- dp$get_site_details_raw()
  output_site_details_cleaned <- dp$get_site_details_cleaning_report()
  output_circuit_details <- dp$get_circuit_details_raw()
  
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_site_details_cleaned, expected_site_details_cleaned, tolerance = 1e-6)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})

testthat::test_that("test calculating duration values works, with batch size
  smaller than number of circuits, more complex examples.",{
    
  timeseries_with_missing_durations <- "data/timeseries_with_missing_durations_two.csv"
  timeseries_expected_out <- "data/timeseries_with_missing_durations_two_expected.csv"
  site_details_path_name <- "data/simple_site_details.csv"
  site_details_cleaned_path_name <- "data/simple_site_details_cleaned.csv"
  circuit_details_path_name <- "data/simple_circuit_details.csv"
  
  expected_timeseries <- load_test_file(timeseries_expected_out)
  expected_site_details <- load_test_file(site_details_path_name)
  expected_site_details_cleaned <- load_test_file(site_details_cleaned_path_name)
  expected_circuit_details <- load_test_file(circuit_details_path_name)
  expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
  expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')
  
  # Create the DBInterface and test creating the database.
  if (file.exists("test.db")) {file.remove("test.db")}
  dp <- DBInterface$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries = timeseries_with_missing_durations,
                    circuit_details = circuit_details_path_name,
                    site_details = site_details_path_name,
                    check_dataset_ids_match=FALSE)
  
  dp$add_postcode_lon_lat_to_database("data/postcode_lon_lat.csv")
  dp$add_manufacturer_mapping_table("data/manufacturer_mapping.csv")
  
  dp$run_data_cleaning_loop(max_chunk_size = 1)
  
  output_timeseries <- dp$get_time_series_data()
  output_site_details <- dp$get_site_details_raw()
  output_site_details_cleaned <- dp$get_site_details_cleaning_report()
  output_circuit_details <- dp$get_circuit_details_raw()

  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_site_details_cleaned, expected_site_details_cleaned, tolerance = 1e-6)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})

testthat::test_that("test calculating duration values works, with batch size 
                    greater than the number of circuits",{
                      
  timeseries_with_missing_durations <- "data/timeseries_with_missing_durations.csv"
  timeseries_expected_out <- "data/simple_timeseries.csv"
  site_details_path_name <- "data/simple_site_details.csv"
  site_details_cleaned_path_name <- "data/simple_site_details_cleaned.csv"
  circuit_details_path_name <- "data/simple_circuit_details.csv"
  
  expected_timeseries <- load_test_file(timeseries_expected_out)
  expected_site_details <- load_test_file(site_details_path_name)
  expected_site_details_cleaned <- load_test_file(site_details_cleaned_path_name)
  expected_circuit_details <- load_test_file(circuit_details_path_name)
  expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
  expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')
  
  if (file.exists("test.db")) {file.remove("test.db")}
  dp <- DBInterface$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries = timeseries_with_missing_durations,
                    circuit_details = circuit_details_path_name,
                    site_details = site_details_path_name,
                    check_dataset_ids_match=FALSE)
  
  dp$add_postcode_lon_lat_to_database("data/postcode_lon_lat.csv")
  dp$add_manufacturer_mapping_table("data/manufacturer_mapping.csv")
  
  dp$run_data_cleaning_loop(max_chunk_size = 3)
  
  output_timeseries <- dp$get_time_series_data()
  output_site_details <- dp$get_site_details_raw()
  output_site_details_cleaned <- dp$get_site_details_cleaning_report()
  output_circuit_details <- dp$get_circuit_details_raw()
  
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_site_details_cleaned, expected_site_details_cleaned, tolerance = 1e-6)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})


testthat::test_that("test calculating duration values works, with batch size 
  equal to the number of circuits",{
    
  timeseries_with_missing_durations <- "data/timeseries_with_missing_durations.csv"
  timeseries_expected_out <- "data/simple_timeseries.csv"
  site_details_path_name <- "data/simple_site_details.csv"
  site_details_cleaned_path_name <- "data/simple_site_details_cleaned.csv"
  circuit_details_path_name <- "data/simple_circuit_details.csv"
  
  expected_timeseries <- load_test_file(timeseries_expected_out)
  expected_site_details <- load_test_file(site_details_path_name)
  expected_site_details_cleaned <- load_test_file(site_details_cleaned_path_name)
  expected_circuit_details <- load_test_file(circuit_details_path_name)
  expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
  expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')
  
  # Create the DBInterface and test creating the database.
  if (file.exists("test.db")) {file.remove("test.db")}
  dp <- DBInterface$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries = timeseries_with_missing_durations,
                    circuit_details = circuit_details_path_name,
                    site_details = site_details_path_name,
                    check_dataset_ids_match=FALSE)
  
  dp$add_postcode_lon_lat_to_database("data/postcode_lon_lat.csv")
  dp$add_manufacturer_mapping_table("data/manufacturer_mapping.csv")
  
  dp$run_data_cleaning_loop(max_chunk_size = 2)
  
  output_timeseries <- dp$get_time_series_data()
  output_site_details <- dp$get_site_details_raw()
  output_site_details_cleaned <- dp$get_site_details_cleaning_report()
  output_circuit_details <- dp$get_circuit_details_raw()
  
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_site_details_cleaned, expected_site_details_cleaned, tolerance = 1e-6)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
  
})

testthat::test_that("test circuit data cleaning, no cleaning required",{
    
    timeseries <- "data/simple_timeseries.csv"
    site_details_path_name <- "data/simple_site_details.csv"
    circuit_details_path_name <- "data/simple_circuit_details.csv"
    circuit_details_path_name_clean <- "data/simple_circuit_details_cleaned.csv"
    
    expected_timeseries <- load_test_file(timeseries)
    expected_site_details <- load_test_file(site_details_path_name)
    expected_circuit_details <- load_test_file(circuit_details_path_name)
    expected_circuit_details <- mutate(expected_circuit_details, manual_droop_compliance = 'Not set')
    expected_circuit_details <- mutate(expected_circuit_details, manual_reconnect_compliance = 'Not set')
    expected_circuit_details_cleaned <- load_test_file(circuit_details_path_name_clean)
    expected_circuit_details_cleaned <- mutate(expected_circuit_details_cleaned, manual_droop_compliance = 'Not set')
    expected_circuit_details_cleaned <- mutate(expected_circuit_details_cleaned, manual_reconnect_compliance = 'Not set')
    
    # Create the DBInterface and test creating the database.
    if (file.exists("test.db")) {file.remove("test.db")}
    dp <- DBInterface$new()
    dp$connect_to_new_database("test.db")
    dp$build_database(timeseries = timeseries,
                      circuit_details = circuit_details_path_name,
                      site_details = site_details_path_name,
                      check_dataset_ids_match=FALSE)
    
    dp$add_postcode_lon_lat_to_database("data/postcode_lon_lat.csv")
    dp$add_manufacturer_mapping_table("data/manufacturer_mapping.csv")
    
    dp$run_data_cleaning_loop(max_chunk_size = 2)
    
    output_timeseries <- dp$get_time_series_data()
    output_site_details <- dp$get_site_details_raw()
    output_circuit_details <- dp$get_circuit_details_raw()
    output_circuit_details_cleaned <- dp$get_circuit_details_cleaning_report()
    
    testthat::expect_equal(output_timeseries, expected_timeseries)
    testthat::expect_equal(output_site_details, expected_site_details)
    testthat::expect_equal(output_circuit_details, expected_circuit_details)
    testthat::expect_equal(output_circuit_details_cleaned, expected_circuit_details_cleaned)
    
  })

