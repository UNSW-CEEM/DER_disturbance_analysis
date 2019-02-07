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
  ac <- c(59, 60, 8)
  pv_installation_year_month <- c("2017-01", "2017-10", "2018-01")
  test_site_details <- data.frame(site_id, s_state, dc, ac,
                                  pv_installation_year_month,
                                  stringsAsFactors = FALSE)
  # Test output data
  site_id <- c(101, 300)
  s_state <- c("NSW", "SA")
  sum_dc <- c(120, 10)
  first_ac <- c(59, 8)
  pv_installation_year_month <- c("2017-01", "2018-01")
  expected_answer <- data.frame(site_id, s_state, pv_installation_year_month,
                                sum_dc, first_ac, stringsAsFactors = FALSE)
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

test_that("Test the circuit details filtering function",{
  # Test input data
  con_type <- c("ac_load_net", "load_air_conditioner", "ac_load", 
                "load_hot_water", "pv_inverter", "pv_inverter_net",
                "pv_site", "pv_site_net")
  id <- c(1001, 1002, 1003, 1000, 1000, 1001, 1002, 1003)
  test_combined_data <- data.frame(con_type, id, stringsAsFactors = FALSE)
  # Test output data
  con_type <- c("pv_inverter_net", "pv_site", "pv_site_net")
  id <- c(1001, 1002, 1003)
  expected_answer <- data.frame(con_type, id, stringsAsFactors = FALSE)
  # Call processing function
  test_combined_data = process_raw_circuit_details(test_combined_data)
  # Test the answer matches the expected answer
  expect_equal(test_combined_data, expected_answer, tolerance=0.0001)
}) 

test_that("Test the standard catergorisation function",{
  # Test input data
  site_id <- c(101, 50, 10002, 89, 567, 111, 1)
  s_state <- c("NSW", "NSW", "SA", "VIC", "QLD", "SA", "TAS")
  sum_dc <- c(60, 60, 10, 60, 60, 10, 11)
  first_ac <- c(60, 60, 10, 60, 60, 10, 11)
  pv_installation_year_month <- c("", NA, "2015-01", "2015-10-09", "2015-11",
                                  "2016-10-09", "2017-10")
  test_site_details <- data.frame(site_id, s_state, sum_dc, first_ac,
                                  pv_installation_year_month,
                                  stringsAsFactors = FALSE)
  # Test output data
  site_id <- c(101, 50, 10002, 89, 567, 111, 1)
  s_state <- c("NSW", "NSW", "SA", "VIC", "QLD", "SA", "TAS")
  sum_dc <- c(60, 60, 10, 60, 60, 10, 11)
  first_ac <- c(60, 60, 10, 60, 60, 10, 11)
  pv_installation_year_month <- c(ymd("2005-01-28"), ymd("2005-01-28"), 
                                  ymd("2015-01-28"), ymd("2015-10-09"),
                                  ymd("2015-11-28"), ymd("2016-10-09"), 
                                  ymd("2017-10-28"))
  Standard_Version <- c("AS4777.3:2005", "AS4777.3:2005", 
                        "AS4777.3:2005", "Transition",
                        "Transition", "AS4777.2:2015", 
                        "AS4777.2:2015")
  expected_answer <- data.frame(site_id, s_state, sum_dc, first_ac,
                                pv_installation_year_month, Standard_Version,
                                stringsAsFactors = FALSE)
  # Call processing function
  processed_site_details = site_catergorisation(test_site_details)
  # Test the answer matches the expected answer
  expect_identical(processed_site_details, expected_answer)
}) 