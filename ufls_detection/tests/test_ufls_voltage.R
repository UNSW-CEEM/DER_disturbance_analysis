context("Testing the DER event analysis UFLS detection based on voltage")


test_that("Test the calculation of average voltage over a window for 1 site" ,{
  # Test input data
  c_id <- c(100, 100, 100)
  v <- c(239, 240, 241)
  input_data <- data.frame(c_id, v)
  out <- calc_average_voltage_per_circuit(input_data)
  expected_output <- data.frame(c_id = c(100), v_mean = c(240))
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the calculation of average voltage over a window for 2 sites" ,{
  # Test input data
  c_id <- c(100, 100, 100, 101, 101)
  v <- c(239, 240, 241, 230, 240)
  input_data <- data.frame(c_id, v)
  out <- calc_average_voltage_per_circuit(input_data)
  expected_output <- data.frame(c_id = c(100, 101), v_mean = c(240, 235))
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a No UFLS Dropout example with one system", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:08:55", "2018-01-01 13:09:55", "2018-01-01 13:10:55", 
          "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", 
          "2018-01-01 13:14:55", "2018-01-01 13:15:55", "2018-01-01 13:16:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
  v <- c(240, 239, 238, 240, 238, 240, 240, 238, 239)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length)
  c_id <- c(1)
  pre_event_v_mean <- c(239)
  post_event_v_mean <- c(239)
  ufls_status_v <- c("No UFLS Dropout")
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a UFLS dropout example with one system", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:08:55", "2018-01-01 13:09:55", "2018-01-01 13:10:55", 
          "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", 
          "2018-01-01 13:14:55", "2018-01-01 13:15:55", "2018-01-01 13:16:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
  v <- c(240, 239, 238, 0, 0, 0, 0, 0, 0)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length)
  c_id <- c(1)
  pre_event_v_mean <- c(239)
  post_event_v_mean <- c(0)
  ufls_status_v <- c("UFLS Dropout")
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a UFLS example with no pre-event timesteps", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", 
          "2018-01-01 13:14:55", "2018-01-01 13:15:55", "2018-01-01 13:16:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1)
  v <- c(240, 239, 238, 241, 242, 240)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length)
  c_id <- c(1)
  pre_event_v_mean <- c(NA_integer_)
  post_event_v_mean <- c(240)
  ufls_status_v <- c(NA_character_)
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a UFLS example with no pre or post event timesteps, with fillna", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", 
          "2018-01-01 13:14:55", "2018-01-01 13:15:55", "2018-01-01 13:16:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1)
  v <- c(NA, NA, NA, NA, NA, NA)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length, fill_nans = TRUE)
  c_id <- c(1)
  pre_event_v_mean <- c(NA_integer_)
  post_event_v_mean <- c(0)
  ufls_status_v <- c(NA_character_)
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a UFLS example with no pre or post event timesteps, no fillna", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", 
          "2018-01-01 13:14:55", "2018-01-01 13:15:55", "2018-01-01 13:16:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1)
  v <- c(NA, NA, NA, NA, NA, NA)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length)
  c_id <- c(1)
  pre_event_v_mean <- c(NA_integer_)
  post_event_v_mean <- c(NA_integer_)
  ufls_status_v <- c(NA_character_)
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a UFLS example with no pre-event timesteps and post-event 0s", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", 
          "2018-01-01 13:14:55", "2018-01-01 13:15:55", "2018-01-01 13:16:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1)
  v <- c(0, 0, 0, 0, 0, 0)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length)
  c_id <- c(1)
  pre_event_v_mean <- c(NA_integer_)
  post_event_v_mean <- c(0)
  ufls_status_v <- c(NA_character_)
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a UFLS example with pre-event values and no post-event timesteps, with fillna", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:08:55", "2018-01-01 13:09:55", "2018-01-01 13:10:55", 
          "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1)
  v <- c(240, 240, 240, NA, NA, NA)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length, fill_nans = TRUE)
  c_id <- c(1)
  pre_event_v_mean <- c(240)
  post_event_v_mean <- c(0)
  ufls_status_v <- c('UFLS Dropout')
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a UFLS example with pre-event values and no post-event timesteps, no fillna", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:08:55", "2018-01-01 13:09:55", "2018-01-01 13:10:55", 
          "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1)
  v <- c(240, 240, 240, NA, NA, NA)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length)
  c_id <- c(1)
  pre_event_v_mean <- c(240)
  post_event_v_mean <- c(NA_integer_)
  ufls_status_v <- c(NA_character_)
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a UFLS example with pre-event 0s and post-event 240", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:08:55", "2018-01-01 13:09:55", "2018-01-01 13:10:55", 
          "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1)
  v <- c(0, 0, 0, 240, 240, 240)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length)
  c_id <- c(1)
  pre_event_v_mean <- c(0)
  post_event_v_mean <- c(240)
  ufls_status_v <- c(NA_character_)
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test a UFLS example with pre-event 10V and post-event 0v", {
  window_length <- 5
  event_time <- "2018-01-01 13:11:55"
  event_time <- as.POSIXct(strptime(event_time, "%Y-%m-%d %H:%M:%S", 
                                    tz="Australia/Brisbane"))
  ts <- c("2018-01-01 13:08:55", "2018-01-01 13:09:55", "2018-01-01 13:10:55", 
          "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  c_id <- c(1, 1, 1, 1, 1, 1)
  v <- c(10, 11, 9, 0, 0, 0)
  input_data <- data.frame(c_id, ts, v, stringsAsFactors = FALSE)
  out <- ufls_detection_voltage(input_data, event_time, window_length)
  c_id <- c(1)
  pre_event_v_mean <- c(10)
  post_event_v_mean <- c(0)
  ufls_status_v <- c(NA_character_)
  expected_output <- data.frame(c_id, pre_event_v_mean, post_event_v_mean, ufls_status_v, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})