context("Testing the data cleaning functions")


test_that("Test remove_outlying_voltages voltage too low", {
  # Setup inputs
  c_id <- c(1, 1, 1)
  ts <- c("2018-01-02 05:56:20", "2018-01-02 03:58:04", "2018-01-02 06:16:17")
  ts <- strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  v <- c(0, 0, 1)
  vmin <- c(0, 0, 1)
  vmax <- c(0, 0, 1)
  vmean <- c(0, 0, 1)
  v_changed <- c(TRUE, TRUE, TRUE)

  test_timeseries <- data.frame(c_id, ts, v, vmin, vmax, vmean, stringsAsFactors = FALSE)
  expected_timeseries <- mutate(test_timeseries, v = NaN, vmin = NaN, vmax = NaN, vmean = NaN)
  expected_timeseries["v_changed"] <- v_changed

  # Call processing function
  output_timeseries <- remove_outlying_voltages(test_timeseries)
  expect_equal(output_timeseries, expected_timeseries)
})

test_that("Test remove_outlying_voltages voltage too high", {
  # Setup inputs
  c_id <- c(1, 1, 1)
  ts <- c("2018-01-02 05:56:20", "2018-01-02 03:58:04", "2018-01-02 06:16:17")
  ts <- strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  v <- c(1200, 1300, 1400)
  vmin <- v
  vmax <- v
  vmean <- v
  v_changed <- c(TRUE, TRUE, TRUE)

  test_timeseries <- data.frame(c_id, ts, v, vmin, vmax, vmean, stringsAsFactors = FALSE)
  expected_timeseries <- mutate(test_timeseries, v = NaN, vmin = NaN, vmax = NaN, vmean = NaN)
  expected_timeseries["v_changed"] <- v_changed

  # Call processing function
  output_timeseries <- remove_outlying_voltages(test_timeseries)

  expect_equal(output_timeseries, expected_timeseries)
})

test_that("Test remove_outlying_voltages voltage mixed", {
  # Setup inputs
  c_id <- c(1, 1, 1)
  ts <- c("2018-01-02 05:56:20", "2018-01-02 03:58:04", "2018-01-02 06:16:17")
  ts <- strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  v <- c(0, 235, 1000)
  vmin <- v
  vmax <- v
  vmean <- v
  v_changed <- c(FALSE, FALSE, FALSE)

  test_timeseries <- data.frame(c_id, ts, v, vmin, vmax, vmean, stringsAsFactors = FALSE)
  expected_timeseries <- test_timeseries
  expected_timeseries["v_changed"] <- v_changed

  # Call processing function
  output_timeseries <- remove_outlying_voltages(test_timeseries)
  expect_equal(output_timeseries, expected_timeseries)
})

test_that("Test remove_outlying_voltages including NaNs", {
  # Setup inputs
  c_id <- c(1, 1, 1)
  ts <- c("2018-01-02 05:56:20", "2018-01-02 03:58:04", "2018-01-02 06:16:17")
  ts <- strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  v <- c(0, 235, 1000)
  vmin <- c(NaN, NaN, NaN)
  vmax <- c(NaN, NaN, "NaN")
  vmean <- c("NaN", "NaN", "NaN")
  v_changed <- c(FALSE, FALSE, FALSE)

  test_timeseries <- data.frame(c_id, ts, v, vmin, vmax, vmean, stringsAsFactors = FALSE)
  expected_timeseries <- data.frame(
      c_id,
      ts,
      v,
      vmin,
      vmax = as.numeric(vmax),
      vmean = as.numeric(vmean),
      stringsAsFactors = FALSE
  )
  expected_timeseries["v_changed"] <- v_changed

  # Call processing function
  output_timeseries <- remove_outlying_voltages(test_timeseries)
  expect_equal(output_timeseries, expected_timeseries)
})

test_that("Test remove_outlying_voltages voltage multiple circuits", {
  # Setup inputs
  c_id <- c(1, 1, 1, 2, 2, 2)
  ts <- c("2018-01-02 05:56:20", "2018-01-02 03:58:04", "2018-01-02 06:16:17")
  ts <- strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  v <- c(0, 235, 1000, 0, 0, 0)
  vmin <- v
  vmax <- v
  vmean <- c(0.1, 0.1 , 0.1, 0, 0, 0)
  test_timeseries <- data.frame(c_id, ts, v, vmin, vmax, vmean, stringsAsFactors = FALSE)

  v <- c(0, 235, 1000, NaN, NaN, NaN)
  vmin <- v
  vmax <- v
  vmean <- c(NaN, NaN, NaN, NaN, NaN, NaN)
  v_changed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  expected_timeseries <- data.frame(c_id, ts, v, vmin, vmax, vmean, v_changed, stringsAsFactors = FALSE)
  # Call processing function
  output_timeseries <- remove_outlying_voltages(test_timeseries)
  expect_equal(output_timeseries, expected_timeseries)
})
