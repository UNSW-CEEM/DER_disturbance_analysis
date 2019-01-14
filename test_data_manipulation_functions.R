context("Testing the DER even analysis underlying data manipulation functions")
source("data_manipulation_functions.R")

test_that("Test the preprocessing of the timeseries data",{
  # Test input data
  c_id <- c("101", "101", "c_id")
  ts <- c("2018-01-01 00:01:00", "2018-01-01 00:02:00", "ts")
  e <- c("1015.0", "-1010.0", "e")
  d <- c("60", "60", "d")
  test_time_series_data <- data.frame(c_id, ts, e, d, stringsAsFactors = FALSE)
  # Test output data
  c_id <- c(101, 101)
  ts <- c(as.POSIXct(strptime("2018-01-01 10:01:00", "%Y-%m-%d %H:%M:%S", 
                              tz="Australia/Brisbane")), 
          as.POSIXct(strptime("2018-01-01 10:02:00", "%Y-%m-%d %H:%M:%S", 
                              tz="Australia/Brisbane")))
  e <- c(1015.0, -1010.0)
  d <- c(60, 60)
  expected_answer <- data.frame(c_id, ts, e, d, stringsAsFactors = FALSE)
  # Call processing function
  processed_time_series = process_raw_time_series_data(test_time_series_data)
  # Test the answer matches the expected answer
  expect_identical(processed_time_series, expected_answer)
})


test_that("Test the preprocessing of the site_details data",{
  # Test input data
  site_id <- c(101, 101, 300)
  s_state <- c("NSW", "NSW", "SA")
  dc <- c(60, 60, 10)
  #pv_installation_year_month <- c("2017-01", "2017-10", "2018-01")
  test_site_details <- data.frame(site_id, s_state, dc,
                                  stringsAsFactors = FALSE)
  # Test output data
  site_id <- c(101, 300)
  s_state <- c("NSW", "SA")
  dc <- c(120, 10)
  #pv_installation_year_month <- c("2017-01", "2018-01")
  expected_answer <- data.frame(site_id, s_state, 
                                dc, stringsAsFactors = FALSE)
  # Call processing function
  processed_site_details = process_raw_site_details(test_site_details)
  # Test the answer matches the expected answer
  expect_identical(processed_site_details, expected_answer)
})

test_that("Test the power calculations",{
  # Test input data
  e <- c(58.5, 102.5, 300, 40, 56, 80)
  polarity <- c(1, 1, 1, -1, -1, -1)
  d <- c(60, 30, 5, 60, 30, 5)
  test_combined_data <- data.frame(e, polarity, d, stringsAsFactors = FALSE)
  # Test output data
  e <- c(58.5, 102.5, 300, 40, 56, 80)
  polarity <- c(1, 1, 1, -1, -1, -1)
  d <- c(60, 30, 5, 60, 30, 5)
  e_polarity <- c(58.5, 102.5, 300, -40, -56, -80)
  power_kW <- c(0.00097500, 0.00341667, 0.06000000, -0.00066667, -0.00186667,
                -0.01600000)
  expected_answer <- data.frame(e, polarity, d, e_polarity, power_kW, 
                                stringsAsFactors = FALSE)
  # Call processing function
  test_combined_data = perform_power_calculations(test_combined_data)
  # Test the answer matches the expected answer
  expect_equal(test_combined_data, expected_answer, tolerance=0.0001)
}) 

test_that("Test the impilict filtering function",{
  # Test input data
  con_type <- c("ac_load_net", "load_air_conditioner", "ac_load", 
                "load_hot_water", "pv_inverter", "pv_inverter_net",
                "pv_site", "pv_site_net", "ac_load_net", "load_air_conditioner", 
                "ac_load", "load_hot_water", "pv_inverter", "pv_inverter_net",
                "pv_site", "pv_site_net")
  dc <- c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 100001, 100001, 
          100001, 100001, 100001, 100001, 100001, 100001)
  test_combined_data <- data.frame(con_type, dc, stringsAsFactors = FALSE)
  # Test output data
  con_type <- c("pv_inverter_net", "pv_site", "pv_site_net")
  dc <- c(1000, 1000, 1000)
  expected_answer <- data.frame(con_type, dc, stringsAsFactors = FALSE)
  # Call processing function
  test_combined_data = implicit_filtering(test_combined_data)
  # Test the answer matches the expected answer
  expect_equal(test_combined_data, expected_answer, tolerance=0.0001)
}) 
