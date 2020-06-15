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
  expected_timeseries <- expected_timeseries[, c('ts', 'c_id', 'd', 'e', 'v', 'f')]
  expected_site_details <- read.csv(file=site_details_path_name, header=TRUE, stringsAsFactors = FALSE)
  expected_circuit_details <- read.csv(file=circuit_details_path_name, header=TRUE, stringsAsFactors = FALSE)
  # Create the DataProcessor and test creating the database.
  if (file.exists("test.db")){file.remove("test.db")}
  dp <- DataProcessor$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries=timeseries_path_name,
                    circuit_details=circuit_details_path_name,
                    site_details=site_details_path_name)
  # Extract the data from the database directly
  library(sqldf)
  output_timeseries <- sqldf::read.csv.sql(
    sql = "select * from timeseries", dbname = "test.db")
  output_site_details <- sqldf::read.csv.sql(
    sql = "select * from site_details_raw", dbname = "test.db")
  output_circuit_details <- sqldf::read.csv.sql(
    sql = "select * from circuit_details_raw", dbname = "test.db")
  # Check if the dataframe created via DataProcessor are equal to those read 
  # directlt from csv.
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
  
})

testthat::test_that("Building works when there are duplicates in ts data.",{
  # Some very simple test data just to test the basic functionality of making
  # the sqlite database.
  timeseries_duplicates_path_name <- "timeseries_with_duplicates.csv"
  timeseries_no_duplicates_path_name <- "smoke_timeseries.csv"
  site_details_path_name <- "smoke_site_details.csv"
  circuit_details_path_name <- "smoke_circuit_details.csv"
  # We expect the output of loading via the database to be the same as if we
  # just read straight from csv.
  expected_timeseries <- read.csv(file=timeseries_no_duplicates_path_name, header=TRUE, stringsAsFactors = FALSE)
  expected_timeseries <- expected_timeseries[, c('ts', 'c_id', 'd', 'e', 'v', 'f')]
  expected_site_details <- read.csv(file=site_details_path_name, header=TRUE, stringsAsFactors = FALSE)
  expected_circuit_details <- read.csv(file=circuit_details_path_name, header=TRUE, stringsAsFactors = FALSE)
  # Create the DataProcessor and test creating the database.
  if (file.exists("test.db")){file.remove("test.db")}
  dp <- DataProcessor$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries=timeseries_duplicates_path_name,
                    circuit_details=circuit_details_path_name,
                    site_details=site_details_path_name)
  # Extract the data from the database directly
  library(sqldf)
  output_timeseries <- sqldf::read.csv.sql(
    sql = "select * from timeseries", dbname = "test.db")
  output_site_details <- sqldf::read.csv.sql(
    sql = "select * from site_details_raw", dbname = "test.db")
  output_circuit_details <- sqldf::read.csv.sql(
    sql = "select * from circuit_details_raw", dbname = "test.db")
  # Check if the dataframe created via DataProcessor are equal to those read 
  # directlt from csv.
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})

testthat::test_that("Building works when there are extra headers in the ts data.",{
  # Some very simple test data just to test the basic functionality of making
  # the sqlite database.
  timeseries_with_extra_header <- "timeseries_with_extra_header.csv"
  timeseries_no_duplicates_path_name <- "smoke_timeseries.csv"
  site_details_path_name <- "smoke_site_details.csv"
  circuit_details_path_name <- "smoke_circuit_details.csv"
  # We expect the output of loading via the database to be the same as if we
  # just read straight from csv.
  expected_timeseries <- read.csv(file=timeseries_no_duplicates_path_name, header=TRUE, stringsAsFactors = FALSE)
  expected_timeseries <- expected_timeseries[, c('ts', 'c_id', 'd', 'e', 'v', 'f')]
  expected_site_details <- read.csv(file=site_details_path_name, header=TRUE, stringsAsFactors = FALSE)
  expected_circuit_details <- read.csv(file=circuit_details_path_name, header=TRUE, stringsAsFactors = FALSE)
  # Create the DataProcessor and test creating the database.
  if (file.exists("test.db")){file.remove("test.db")}
  dp <- DataProcessor$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries=timeseries_with_extra_header,
                    circuit_details=circuit_details_path_name,
                    site_details=site_details_path_name)
  dp$drop_repeated_headers()
  # Extract the data from the database directly
  library(sqldf)
  output_timeseries <- sqldf::read.csv.sql(
    sql = "select * from timeseries", dbname = "test.db")
  output_site_details <- sqldf::read.csv.sql(
    sql = "select * from site_details_raw", dbname = "test.db")
  output_circuit_details <- sqldf::read.csv.sql(
    sql = "select * from circuit_details_raw", dbname = "test.db")
  # Check if the dataframe created via DataProcessor are equal to those read 
  # directlt from csv.
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})

testthat::test_that("test calculating duration values works, with batch size
                    smaller than number of circuits.",{
  # Some very simple test data just to test the basic functionality of making
  # the sqlite database.
  timeseries_with_missing_durations <- "timeseries_with_missing_durations.csv"
  timeseries_expected_out <- "smoke_timeseries.csv"
  site_details_path_name <- "smoke_site_details.csv"
  circuit_details_path_name <- "smoke_circuit_details.csv"
  # We expect the output of loading via the database to be the same as if we
  # just read straight from csv.
  expected_timeseries <- read.csv(file=timeseries_expected_out, header=TRUE, 
                                  stringsAsFactors = FALSE)
  expected_timeseries <- expected_timeseries[, c('ts', 'c_id', 'd', 'e', 'v', 'f')]
  expected_site_details <- read.csv(file=site_details_path_name, header=TRUE, 
                                    stringsAsFactors = FALSE)
  expected_circuit_details <- read.csv(file=circuit_details_path_name, header=TRUE, 
                                       stringsAsFactors = FALSE)
  # Create the DataProcessor and test creating the database.
  if (file.exists("test.db")){file.remove("test.db")}
  dp <- DataProcessor$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries=timeseries_with_missing_durations,
                    circuit_details=circuit_details_path_name,
                    site_details=site_details_path_name)
  dp$run_data_cleaning_loop(max_chunk_size=2, calc_duration_values=TRUE)
  # Extract the data from the database directly
  library(sqldf)
  output_timeseries <- sqldf::read.csv.sql(
    sql = "select * from timeseries", dbname = "test.db")
  output_site_details <- sqldf::read.csv.sql(
    sql = "select * from site_details_raw", dbname = "test.db")
  output_circuit_details <- sqldf::read.csv.sql(
    sql = "select * from circuit_details_raw", dbname = "test.db")
  # Check if the dataframe created via DataProcessor are equal to those read 
  # directlt from csv.
  output_timeseries <- output_timeseries[with(output_timeseries, order(c_id, ts)), ]
  print(output_timeseries)
  rownames(output_timeseries) <- NULL
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})

testthat::test_that("test calculating duration values works, with batch size
  smaller than number of circuits, more complex examples.",{
  # Some very simple test data just to test the basic functionality of making
  # the sqlite database.
  timeseries_with_missing_durations <- "timeseries_with_missing_durations_two.csv"
  timeseries_expected_out <- "timeseries_with_missing_durations_two_expected.csv"
  site_details_path_name <- "smoke_site_details.csv"
  circuit_details_path_name <- "smoke_circuit_details.csv"
  # We expect the output of loading via the database to be the same as if we
  # just read straight from csv.
  expected_timeseries <- read.csv(file=timeseries_expected_out, header=TRUE, 
                                  stringsAsFactors = FALSE)
  expected_timeseries <- expected_timeseries[, c('ts', 'c_id', 'd', 'e', 'v', 'f')]
  expected_site_details <- read.csv(file=site_details_path_name, header=TRUE, 
                                    stringsAsFactors = FALSE)
  expected_circuit_details <- read.csv(file=circuit_details_path_name, header=TRUE, 
                                       stringsAsFactors = FALSE)
  # Create the DataProcessor and test creating the database.
  if (file.exists("test.db")){file.remove("test.db")}
  dp <- DataProcessor$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries=timeseries_with_missing_durations,
                    circuit_details=circuit_details_path_name,
                    site_details=site_details_path_name)
  dp$run_data_cleaning_loop(max_chunk_size=2, calc_duration_values=TRUE)
  # Extract the data from the database directly
  library(sqldf)
  output_timeseries <- sqldf::read.csv.sql(
    sql = "select * from timeseries", dbname = "test.db")
  output_site_details <- sqldf::read.csv.sql(
    sql = "select * from site_details_raw", dbname = "test.db")
  output_circuit_details <- sqldf::read.csv.sql(
    sql = "select * from circuit_details_raw", dbname = "test.db")
  # Check if the dataframe created via DataProcessor are equal to those read 
  # directlt from csv.  
  output_timeseries <- output_timeseries[with(output_timeseries, order(c_id, ts)), ]
  rownames(output_timeseries) <- NULL
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
})

testthat::test_that("test calculating duration values works, with batch size 
                    greater than the number of circuits",{
  # Some very simple test data just to test the basic functionality of making
  # the sqlite database.
  timeseries_with_missing_durations <- "timeseries_with_missing_durations.csv"
  timeseries_expected_out <- "smoke_timeseries.csv"
  site_details_path_name <- "smoke_site_details.csv"
  circuit_details_path_name <- "smoke_circuit_details.csv"
  # We expect the output of loading via the database to be the same as if we
  # just read straight from csv.
  expected_timeseries <- read.csv(file=timeseries_expected_out, header=TRUE, 
                                  stringsAsFactors = FALSE)
  expected_timeseries <- expected_timeseries[, c('ts', 'c_id', 'd', 'e', 'v', 'f')]
  expected_site_details <- read.csv(file=site_details_path_name, header=TRUE, 
                                    stringsAsFactors = FALSE)
  expected_circuit_details <- read.csv(file=circuit_details_path_name, header=TRUE, 
                                       stringsAsFactors = FALSE)
  # Create the DataProcessor and test creating the database.
  if (file.exists("test.db")){file.remove("test.db")}
  dp <- DataProcessor$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries=timeseries_with_missing_durations,
                    circuit_details=circuit_details_path_name,
                    site_details=site_details_path_name)
  dp$run_data_cleaning_loop(max_chunk_size=2, calc_duration_values=TRUE)
  # Extract the data from the database directly
  library(sqldf)
  output_timeseries <- sqldf::read.csv.sql(
    sql = "select * from timeseries", dbname = "test.db")
  output_site_details <- sqldf::read.csv.sql(
    sql = "select * from site_details_raw", dbname = "test.db")
  output_circuit_details <- sqldf::read.csv.sql(
    sql = "select * from circuit_details_raw", dbname = "test.db")
  # Check if the dataframe created via DataProcessor are equal to those read 
  # directlt from csv.
  output_timeseries <- output_timeseries[with(output_timeseries, order(c_id, ts)), ]
  rownames(output_timeseries) <- NULL
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
  
})


testthat::test_that("test calculating duration values works, with batch size 
  equal to the number of circuits",{
  # Some very simple test data just to test the basic functionality of making
  # the sqlite database.
  timeseries_with_missing_durations <- "timeseries_with_missing_durations.csv"
  timeseries_expected_out <- "smoke_timeseries.csv"
  site_details_path_name <- "smoke_site_details.csv"
  circuit_details_path_name <- "smoke_circuit_details.csv"
  # We expect the output of loading via the database to be the same as if we
  # just read straight from csv.
  expected_timeseries <- read.csv(file=timeseries_expected_out, 
                                  header=TRUE, stringsAsFactors = FALSE)
  expected_timeseries <- expected_timeseries[, c('ts', 'c_id', 'd', 'e', 'v', 'f')]
  expected_site_details <- read.csv(file=site_details_path_name, header=TRUE, 
                                    stringsAsFactors = FALSE)
  expected_circuit_details <- read.csv(file=circuit_details_path_name, header=TRUE, 
                                       stringsAsFactors = FALSE)
  # Create the DataProcessor and test creating the database.
  if (file.exists("test.db")){file.remove("test.db")}
  dp <- DataProcessor$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries=timeseries_with_missing_durations,
                    circuit_details=circuit_details_path_name,
                    site_details=site_details_path_name)
  dp$run_data_cleaning_loop(max_chunk_size=2, calc_duration_values=TRUE)
  # Extract the data from the database directly
  library(sqldf)
  output_timeseries <- sqldf::read.csv.sql(
    sql = "select * from timeseries", dbname = "test.db")
  output_site_details <- sqldf::read.csv.sql(
    sql = "select * from site_details_raw", dbname = "test.db")
  output_circuit_details <- sqldf::read.csv.sql(
    sql = "select * from circuit_details_raw", dbname = "test.db")
  # Check if the dataframe created via DataProcessor are equal to those read 
  # directlt from csv.
  output_timeseries <- output_timeseries[with(output_timeseries, order(c_id, ts)), ]
  rownames(output_timeseries) <- NULL
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
  
})

testthat::test_that("Processing site_details.",{
  # Some very simple test data just to test the basic functionality of making
  # the sqlite database.
  timeseries_path_name <- "smoke_timeseries.csv"
  site_details_path_name <- "smoke_site_details.csv"
  site_details_processed_path_name <- "smoke_site_details_processed.csv"
  circuit_details_path_name <- "smoke_circuit_details.csv"
  # We expect the output of loading via the database to be the same as if we
  # just read straight from csv.
  expected_timeseries <- read.csv(file=timeseries_path_name, header=TRUE, 
                                  stringsAsFactors = FALSE)
  expected_timeseries <- expected_timeseries[, c('ts', 'c_id', 'd', 'e', 'v', 'f')]
  expected_site_details <- read.csv(file=site_details_processed_path_name,
                                    header=TRUE, stringsAsFactors = FALSE)
  expected_circuit_details <- read.csv(file=circuit_details_path_name, 
                                       header=TRUE, stringsAsFactors = FALSE)
  # Create the DataProcessor and test creating the database.
  if (file.exists("test.db")){file.remove("test.db")}
  dp <- DataProcessor$new()
  dp$connect_to_new_database("test.db")
  dp$build_database(timeseries=timeseries_path_name,
                    circuit_details=circuit_details_path_name,
                    site_details=site_details_path_name)
  dp$create_processed_copy_of_site_details()
  # Extract the data from the database directly
  library(sqldf)
  output_timeseries <- sqldf::read.csv.sql(
    sql = "select * from timeseries", dbname = "test.db")
  output_site_details <- sqldf::read.csv.sql(
    sql = "select * from site_details_raw", dbname = "test.db")
  output_circuit_details <- sqldf::read.csv.sql(
    sql = "select * from circuit_details_raw", dbname = "test.db")
  # Check if the dataframe created via DataProcessor are equal to those read 
  # directlt from csv.
  testthat::expect_equal(output_timeseries, expected_timeseries)
  testthat::expect_equal(output_site_details, expected_site_details)
  testthat::expect_equal(output_circuit_details, expected_circuit_details)
  
})
