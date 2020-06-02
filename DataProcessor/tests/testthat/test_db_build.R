# Do not run this file directly, run testthat.R

testthat::context("Testing the creation of a database from csv files.")

testthat::test_that("Building works when no column aliases are required.",{
  # Some very simple test data just to test the basic functionality of making
  # the sqlite database.
  timeseries_path_name <- "smoke_timeseries.csv"
  site_details_path_name <- "smoke_site_details.csv"
  circuit_details_path_name <- "smoke_circuit_details.csv"
  # We expect the output of loading via the database to be the same as if we
  # just read straight from csv.
  expected_timeseries <- read.csv(file=timeseries_path_name, header=TRUE, stringsAsFactors = FALSE)
  expected_site_details <- read.csv(file=site_details_path_name, header=TRUE, stringsAsFactors = FALSE)
  expected_circuit_details <- read.csv(file=circuit_details_path_name, header=TRUE, stringsAsFactors = FALSE)
  # Create the DataProcessor and test creating the database.
  if (file.exists("testdb")){file.remove("testdb")}
  dp <- DataProcessor$new()
  dp$connect_to_new_database("testdb")
  dp$build_database(timeseries=timeseries_path_name,
                    circuit_details=circuit_details_path_name,
                    site_details=site_details_path_name)
  # Extract the data from the database directly
  library(sqldf)
  output_timeseries <- sqldf::read.csv.sql(
    sql = "select * from timeseries", dbname = "testdb")
  output_site_details <- sqldf::read.csv.sql(
    sql = "select * from site_details_raw", dbname = "testdb")
  output_circuit_details <- sqldf::read.csv.sql(
    sql = "select * from circuit_details_raw", dbname = "testdb")
  # Check if the dataframe created via DataProcessor are equal to those read 
  # directlt from csv.
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
  
})