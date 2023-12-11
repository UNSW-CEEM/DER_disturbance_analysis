context("Testing the DER event analysis response categorisation function")

test_that("Test the calculation of number of consecutive zeros, less than 0.1 at t0 returns 0", {
  # Test input data
  p <- c(0.001, 1, 1, 0, 0, 1)
  out <- num_consecutive_zeros(p)
  expected_output <- 0
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calculation of number of consecutive zeros, 2 consecutive zeros", {
  # Test input data
  p <- c(1, 1, 1, 0, 0, 1)
  out <- num_consecutive_zeros(p)
  expected_output <- 2
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calculation of number of consecutive zeros, 2 at the start", {
  # Test input data
  p <- c(1, 0, 0, 1, 1, 1)
  out <- num_consecutive_zeros(p)
  expected_output <- 2
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calculation of number of consecutive zeros, 1 at the start", {
  # Test input data
  p <- c(1, 0, 1, 1, 1, 1)
  out <- num_consecutive_zeros(p)
  expected_output <- 1
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calculation of number of consecutive zeros, 2 non consecutive", {
  # Test input data
  p <- c(1, 1, 0, 1, 0, 1)
  out <- num_consecutive_zeros(p)
  expected_output <- 1
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calculation of number of consecutive zeros, 1 at the end", {
  # Test input data
  p <- c(1, 1, 1, 1, 1, 0)
  out <- num_consecutive_zeros(p)
  expected_output <- 1
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the sub categorisation function", {
  # Test input data
  window_length = 5
  d <- c(60, 60, 60, 60, 60, 60, 5, 5, 5, 5, 5, 5)
  event_power <- c(10.5, 0.01, 11.2, 11.2, 10.1, 10.1, 10.5, 0.01, 11.2, 11.2, 10.1, 10.1)
  min_norm_power <- c(0.5, 0.5, 0.01, 0.01, 0.96, 0.96, 0.5, 0.5, 0.01, 0.01, 0.96, 0.96)
  num_con_zeros <- c(0, 0, 1, 2, 0, 0, 0, 0, 1, 2, 0, 0)
  num_data_points <- c(3, 4, 5, 5, 5, 4, 48, 47, 60, 60, 60, 59)
  first_ts <- c(
    "sampled",
    "sampled",
    "sampled",
    "not sampled",
    "sampled",
    "sampled",
    "sampled",
    "sampled",
    "sampled",
    "sampled",
    "not sampled",
    "sampled"
  )
  input_data <- data.frame(
    d,
    event_power,
    min_norm_power,
    num_con_zeros,
    num_data_points,
    first_ts,
    stringsAsFactors = FALSE
  )
  NED_threshold_pct <- 0.8
  out <- categorise_by_response(input_data, window_length, NED_threshold_pct)
  d <- c(60, 60, 60, 60, 60, 60, 5, 5, 5, 5, 5, 5)
  event_power <- c(10.5, 0.01, 11.2, 11.2, 10.1, 10.1, 10.5, 0.01, 11.2, 11.2, 10.1, 10.1)
  min_norm_power <- c(0.5, 0.5, 0.01, 0.01, 0.96, 0.96, 0.5, 0.5, 0.01, 0.01, 0.96, 0.96)
  num_con_zeros <- c(0, 0, 1, 2, 0, 0, 0, 0, 1, 2, 0, 0)
  num_data_points <- c(3, 4, 5, 5, 5, 4, 48, 47, 60, 60, 60, 59)
  response_category <- c(
    "6 Not enough data",
    "5 Off at t0",
    "3 Drop to Zero",
    "6 Not enough data",
    "1 Ride Through",
    "1 Ride Through",
    "2 Curtail",
    "6 Not enough data",
    "3 Drop to Zero",
    "4 Disconnect",
    "6 Not enough data",
    "1 Ride Through"
  )
  expected_output <- data.frame(
    d,
    event_power,
    min_norm_power,
    num_con_zeros,
    num_data_points,
    first_ts,
    response_category,
    stringsAsFactors = FALSE
  )
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test a simple example with one systems through high level function", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  ts <- c(
    "2018-01-01 13:11:55",
    "2018-01-01 13:12:55",
    "2018-01-01 13:13:55",
    "2018-01-01 13:14:55",
    "2018-01-01 13:15:55",
    "2018-01-01 13:16:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(60, 60, 60, 60, 60, 60)
  c_id <- c("1", "1", "1", "1", "1", "1")
  clean <- c("raw", "raw", "raw", "raw", "raw", "raw")
  power_kW <- c(10, 10, 10, 10, 10, 10)
  input_data <- data.frame(c_id, ts, power_kW, d, clean, stringsAsFactors = FALSE)
  NED_threshold_pct <- 0.8
  out <- categorise_response(input_data, event_time, window_length, NED_threshold_pct)
  ts <- c(
    "2018-01-01 13:11:55",
    "2018-01-01 13:12:55",
    "2018-01-01 13:13:55",
    "2018-01-01 13:14:55",
    "2018-01-01 13:15:55",
    "2018-01-01 13:16:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(60, 60, 60, 60, 60, 60)
  c_id <- c("1", "1", "1", "1", "1", "1")
  clean <- c("raw", "raw", "raw", "raw", "raw", "raw")
  power_kW <- c(10, 10, 10, 10, 10, 10)
  response_category <- c(
    "1 Ride Through",
    "1 Ride Through",
    "1 Ride Through",
    "1 Ride Through",
    "1 Ride Through",
    "1 Ride Through"
  )
  expected_output <- data.frame(c_id, ts, power_kW, d, clean, response_category, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test a simple example with one systems with no pre event interval", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  ts <- c(
    "2018-01-01 13:10:55",
    "2018-01-01 13:12:55",
    "2018-01-01 13:13:55",
    "2018-01-01 13:14:55",
    "2018-01-01 13:15:55",
    "2018-01-01 13:16:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(60, 60, 60, 60, 60, 60)
  c_id <- c("1", "1", "1", "1", "1", "1")
  clean <- c("raw", "raw", "raw", "raw", "raw", "raw")
  power_kW <- c(10, 10, 10, 10, 10, 10)
  input_data <- data.frame(c_id, ts, power_kW, d, clean, stringsAsFactors = FALSE)
  NED_threshold_pct <- 0.8
  out <- categorise_response(input_data, event_time, window_length, NED_threshold_pct)
  ts <- c(
    "2018-01-01 13:10:55",
    "2018-01-01 13:12:55",
    "2018-01-01 13:13:55",
    "2018-01-01 13:14:55",
    "2018-01-01 13:15:55",
    "2018-01-01 13:16:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(60, 60, 60, 60, 60, 60)
  c_id <- c("1", "1", "1", "1", "1", "1")
  clean <- c("raw", "raw", "raw", "raw", "raw", "raw")
  power_kW <- c(10, 10, 10, 10, 10, 10)
  response_category <- c(
    "6 Not enough data",
    "6 Not enough data",
    "6 Not enough data",
    "6 Not enough data",
    "6 Not enough data",
    "6 Not enough data"
  )

  expected_output <- data.frame(c_id, ts, power_kW, d, clean, response_category, stringsAsFactors = FALSE)

  expect_equal(out, expected_output, tolerance = 0.001)
})


test_that("Test a simple example with one system with data points above threshold", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  ts <- c(
    "2018-01-01 13:11:55",
    "2018-01-01 13:12:55",
    "2018-01-01 13:13:55",
    "2018-01-01 13:15:55",
    "2018-01-01 13:16:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(60, 60, 60, 60, 60)
  c_id <- c("1", "1", "1", "1", "1")
  clean <- c("raw", "raw", "raw", "raw", "raw")
  power_kW <- c(10, 10, 10, 10, 10)
  input_data <- data.frame(c_id, ts, power_kW, d, clean, stringsAsFactors = FALSE)
  NED_threshold_pct <- 0.8
  out <- categorise_response(input_data, event_time, window_length, NED_threshold_pct)
  ts <- c(
    "2018-01-01 13:11:55",
    "2018-01-01 13:12:55",
    "2018-01-01 13:13:55",
    "2018-01-01 13:15:55",
    "2018-01-01 13:16:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(60, 60, 60, 60, 60)
  c_id <- c("1", "1", "1", "1", "1")
  clean <- c("raw", "raw", "raw", "raw", "raw")
  power_kW <- c(10, 10, 10, 10, 10)
  response_category <- c("1 Ride Through", "1 Ride Through", "1 Ride Through", "1 Ride Through", "1 Ride Through")

  expected_output <- data.frame(c_id, ts, power_kW, d, clean, response_category, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test a simple example with one system with data points below threshold", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  ts <- c("2018-01-01 13:11:55", "2018-01-01 13:15:55", "2018-01-01 13:16:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(60, 60, 60)
  c_id <- c("1", "1", "1")
  clean <- c("raw", "raw", "raw")
  power_kW <- c(10, 10, 10)
  input_data <- data.frame(c_id, ts, power_kW, d, clean, stringsAsFactors = FALSE)
  NED_threshold_pct <- 0.8
  out <- categorise_response(input_data, event_time, window_length, NED_threshold_pct)
  ts <- c("2018-01-01 13:11:55", "2018-01-01 13:15:55", "2018-01-01 13:16:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(60, 60, 60)
  c_id <- c("1", "1", "1")
  clean <- c("raw", "raw", "raw")
  power_kW <- c(10, 10, 10)
  response_category <- c("6 Not enough data", "6 Not enough data", "6 Not enough data")

  expected_output <- data.frame(c_id, ts, power_kW, d, clean, response_category, stringsAsFactors = FALSE)

  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test a simple example with one system with data points below threshold using 5s data", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  ts <- c(
    "2018-01-01 13:11:55",
    "2018-01-01 13:12:00",
    "2018-01-01 13:12:05",
    "2018-01-01 13:12:05",
    "2018-01-01 13:13:15",
    "2018-01-01 13:14:00"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(5,5,5,5,5,5)
  c_id <- c("1", "1", "1", "1", "1", "1")
  clean <- c("raw", "raw", "raw", "raw", "raw", "raw")
  power_kW <- c(10, 10, 10, 10, 10, 10)
  input_data <- data.frame(c_id, ts, power_kW, d, clean, stringsAsFactors = FALSE)
  NED_threshold_pct <- 0.8
  out <- categorise_response(input_data, event_time, window_length, NED_threshold_pct)
  ts <- c(
    "2018-01-01 13:11:55",
    "2018-01-01 13:12:00",
    "2018-01-01 13:12:05",
    "2018-01-01 13:12:05",
    "2018-01-01 13:13:15",
    "2018-01-01 13:14:00"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  d <- c(5,5,5,5,5,5)
  c_id <- c("1", "1", "1", "1", "1", "1")
  clean <- c("raw", "raw", "raw", "raw", "raw", "raw")
  power_kW <- c(10, 10, 10, 10, 10, 10)
  response_category <- c(
    "6 Not enough data",
    "6 Not enough data",
    "6 Not enough data",
    "6 Not enough data",
    "6 Not enough data",
    "6 Not enough data"
  )

  expected_output <- data.frame(c_id, ts, power_kW, d, clean, response_category, stringsAsFactors = FALSE)

  expect_equal(out, expected_output, tolerance = 0.001)
})
