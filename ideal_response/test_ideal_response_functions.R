library(lubridate)
library(dplyr)
library(assertthat)
library(testthat)
source("ideal_response_functions.R")
context("Testing the DER event analysis distance and zone functions")

test_that("Test the calculation of the ideal inverter response to over frequency, simple case", {
  ts <- c(
    "2018-01-01 13:11:50",
    "2018-01-01 13:11:51",
    "2018-01-01 13:11:52",
    "2018-01-01 13:11:53",
    "2018-01-01 13:11:54",
    "2018-01-01 13:11:55",
    "2018-01-01 13:11:56",
    "2018-01-01 13:11:57"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(50.0, 50.18, 50.25, 50.3, 51.5, 50.5, 50.5, 50.5)
  input_data <- data.frame(ts, f, stringsAsFactors = FALSE)
  out <- ideal_response(input_data)
  ts <- c(
    "2018-01-01 13:11:52",
    "2018-01-01 13:11:53",
    "2018-01-01 13:11:54",
    "2018-01-01 13:11:55",
    "2018-01-01 13:11:56",
    "2018-01-01 13:11:57"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(50.25, 50.3, 51.5, 50.5, 50.5, 50.5)
  norm_power <- c(1.0, 0.97142857142, 0.28571428571, 0.28571428571, 0.28571428571, 0.28571428571)
  expected_output <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calculation of the ideal inverter response to over frequency, don't go to zero for just one reading
           of f=52", {
  ts <- c(
    "2018-01-01 13:11:50",
    "2018-01-01 13:11:51",
    "2018-01-01 13:11:52",
    "2018-01-01 13:11:53",
    "2018-01-01 13:11:54",
    "2018-01-01 13:11:55",
    "2018-01-01 13:11:56",
    "2018-01-01 13:11:57"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(50.0, 50.0, 52.0, 50.3, 51.5, 50.5, 50.5, 50.5)
  input_data <- data.frame(ts, f, stringsAsFactors = FALSE)
  out <- ideal_response(input_data)
  ts <- c(
    "2018-01-01 13:11:52",
    "2018-01-01 13:11:53",
    "2018-01-01 13:11:54",
    "2018-01-01 13:11:55",
    "2018-01-01 13:11:56",
    "2018-01-01 13:11:57"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(52.0, 50.3, 51.5, 50.5, 50.5, 50.5)
  norm_power <- c(1.0, 0.97142857142, 0.28571428571, 0.28571428571, 0.28571428571, 0.28571428571)
  expected_output <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calculation of the ideal inverter response to over frequency, go to zero for just 2 readings
           of f=52", {
  ts <- c(
    "2018-01-01 13:11:50",
    "2018-01-01 13:11:51",
    "2018-01-01 13:11:52",
    "2018-01-01 13:11:53",
    "2018-01-01 13:11:54",
    "2018-01-01 13:11:55",
    "2018-01-01 13:11:56",
    "2018-01-01 13:11:57"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(50.0, 50.0, 52.0, 52.0, 51.5, 50.5, 50.5, 50.5)
  input_data <- data.frame(ts, f, stringsAsFactors = FALSE)
  out <- ideal_response(input_data)
  ts <- c(
    "2018-01-01 13:11:52",
    "2018-01-01 13:11:53",
    "2018-01-01 13:11:54",
    "2018-01-01 13:11:55",
    "2018-01-01 13:11:56",
    "2018-01-01 13:11:57"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(52.0, 52.0, 51.5, 50.5, 50.5, 50.5)
  norm_power <- c(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  expected_output <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calculation of the ideal inverter response to over frequency, no response defined after 60s past
          return to normal frequency", {
  ts <- c(
    "2018-01-01 13:11:50",
    "2018-01-01 13:11:51",
    "2018-01-01 13:11:52",
    "2018-01-01 13:12:51",
    "2018-01-01 13:12:52",
    "2018-01-01 13:12:53",
    "2018-01-01 13:12:54",
    "2018-01-01 13:12:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08, 50.05, 50.05, 50.05, 50.05)
  input_data <- data.frame(ts, f, stringsAsFactors = FALSE)
  out <- ideal_response(input_data)
  ts <- c("2018-01-01 13:11:50", "2018-01-01 13:11:51", "2018-01-01 13:11:52", "2018-01-01 13:12:51")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08)
  norm_power <- c(1.0, 0.0, 0.0, 0.0)
  expected_output <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the downsampling of the 1 s Ideal response, simple", {
  ts <- c(
    "2018-01-01 13:10:56",
    "2018-01-01 13:11:51",
    "2018-01-01 13:11:55",
    "2018-01-01 13:11:56",
    "2018-01-01 13:12:52",
    "2018-01-01 13:12:53",
    "2018-01-01 13:12:54",
    "2018-01-01 13:12:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08, 50.05, 50.05, 50.05, 50.05)
  norm_power <- c(152.0, 152.0, 150.15, 150.08, 155.05, 157.05, 159.05, 151.05)
  input_data <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  out <- down_sample_1s(
    input_data,
    60,
    as.POSIXct(strptime("2018-01-01 13:11:55", "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  )
  time_group <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(50.15, 50.05)
  norm_power <- c(151.3833, 154.456)
  expected_output <- data.frame(time_group, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the downsampling of the 1 s Ideal response, last minute with incomplete data", {
  ts <- c(
    "2018-01-01 13:10:56",
    "2018-01-01 13:11:51",
    "2018-01-01 13:11:55",
    "2018-01-01 13:12:51",
    "2018-01-01 13:12:52",
    "2018-01-01 13:12:53",
    "2018-01-01 13:12:54",
    "2018-01-01 13:12:54"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08, 50.05, 50.05, 50.05, 50.05)
  norm_power <- c(152.0, 152.0, 150.15, 150.08, 155.05, 157.05, 159.05, 151.05)
  input_data <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  out <- down_sample_1s(
    input_data,
    60,
    as.POSIXct(strptime("2018-01-01 13:11:55", "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  )
  time_group <- c("2018-01-01 13:11:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(50.15)
  norm_power <- c(151.3833)
  expected_output <- data.frame(time_group, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the downsampling of the 1 s Ideal response, first minute with incomplete data", {
  ts <- c(
    "2018-01-01 13:11:50",
    "2018-01-01 13:11:51",
    "2018-01-01 13:11:55",
    "2018-01-01 13:11:56",
    "2018-01-01 13:12:52",
    "2018-01-01 13:12:53",
    "2018-01-01 13:12:54",
    "2018-01-01 13:12:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08, 50.05, 50.05, 50.05, 50.05)
  norm_power <- c(152.0, 152.0, 150.15, 150.08, 155.05, 157.05, 159.05, 151.05)
  input_data <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  out <- down_sample_1s(
    input_data,
    60,
    as.POSIXct(strptime("2018-01-01 13:11:55", "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  )
  time_group <- c("2018-01-01 13:12:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  f <- c(50.05)
  norm_power <- c(154.456)
  expected_output <- data.frame(time_group, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calculation of the error metric on a site basis", {
  site_id <- c(
    "100", "100", "100",
    "100", "100", "100",
    "101", "101", "101",
    "102", "102", "102",
    "103", "103", "103"
  )
  c_id <- c(
    "1000", "1000", "1000",
    "1001", "1001", "1001",
    "1011", "1011", "1011",
    "1021", "1021", "1021",
    "1031", "1031", "1031"
  )
  ts <- c(
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  Site_Event_Normalised_Power_kW <- c(
    1, 0.9, 0.9,
    1, 0.9, 0.9,
    1.1, 0.9, 0.9,
    1, 1.1, 1.2,
    1, 0.975, 0.975
  )
  input_data <- data.frame(ts, site_id, c_id, Site_Event_Normalised_Power_kW, stringsAsFactors = FALSE)
  time_group <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  norm_power <- c(1, 0.95, 0.95)
  ideal_response_downsampled <- data.frame(time_group, norm_power, stringsAsFactors = FALSE)
  out <- calc_error_metric(input_data, ideal_response_downsampled)
  site_id <- c("100", "101", "102", "103")
  abs_percent_diff_actual_cf_ideal <- c(0.03508772, 0.06842105, 0.1403509, 0.01754386)
  percent_diff_actual_cf_ideal <- c(-0.03508772, -0.001754386, 0.1403509, 0.01754386)
  min_diff <- c(-0.05263158, -0.05263158, 0.0, 0.0)
  max_diff <- c(0.0, 0.1, 0.2631579, 0.02631579)
  below_spec <- c(1, 0, 0, 0)
  above_spec <- c(0, 0, 1, 1)
  mixed_wrt_spec <- c(0, 1, 0, 0)
  combined_error_metric <- c(-0.03508772, 0.06842105, 0.1403509, 0.01754386)
  expected_output <- data.frame(
    site_id,
    min_diff,
    max_diff,
    abs_percent_diff_actual_cf_ideal,
    percent_diff_actual_cf_ideal,
    mixed_wrt_spec,
    below_spec,
    above_spec,
    combined_error_metric,
    stringsAsFactors = FALSE
  )
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the categorisation of sites based on error metrics ", {
  site_id <- c("100", "101", "102", "103")
  abs_percent_diff_actual_cf_ideal <- c(0.03508772, 0.06842105, 0.1403509, 0.01754386)
  percent_diff_actual_cf_ideal <- c(-0.03508772, -0.001754386, 0.1403509, 0.01754386)
  min_diff <- c(-0.05263158, -0.05263158, 0.0, 0.0)
  max_diff <- c(0.0, 0.1, 0.2631579, 0.02631579)
  below_spec <- c(1, 0, 0, 0)
  above_spec <- c(0, 0, 1, 1)
  mixed_wrt_spec <- c(0, 1, 0, 0)
  combined_error_metric <- c(-0.03508772, 0.06842105, 0.1403509, 0.01754386)
  input_data <- data.frame(
    site_id,
    min_diff,
    max_diff,
    abs_percent_diff_actual_cf_ideal,
    percent_diff_actual_cf_ideal,
    mixed_wrt_spec,
    below_spec,
    above_spec,
    combined_error_metric,
    stringsAsFactors = FALSE
  )
  time_group <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  norm_power <- c(1, 0.95, 0.95)
  ideal_response_downsampled <- data.frame(time_group, norm_power, stringsAsFactors = FALSE)
  out <- calc_compliance_status(input_data, calc_threshold_error(ideal_response_downsampled))
  compliance_status <- c("Compliant", "Ambigous", "Non Compliant", "Above Ideal Response")
  expected_output <- data.frame(
    site_id,
    min_diff,
    max_diff,
    abs_percent_diff_actual_cf_ideal,
    percent_diff_actual_cf_ideal,
    mixed_wrt_spec,
    below_spec,
    above_spec,
    combined_error_metric,
    compliance_status,
    stringsAsFactors = FALSE
  )
  expect_equal(out, expected_output, tolerance = 0.001)
})

test_that("Test the calc of error metrics and categorisation", {
  site_id <- c(
    "100", "100", "100",
    "100", "100", "100",
    "101", "101", "101",
    "102", "102", "102",
    "103", "103", "103"
  )
  c_id <- c(
    "1000", "1000", "1000",
    "1001", "1001", "1001",
    "1011", "1011", "1011",
    "1021", "1021", "1021",
    "1031", "1031", "1031"
  )
  ts <- c(
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  Site_Event_Normalised_Power_kW <- c(
    1, 0.9, 0.9,
    1, 0.9, 0.9,
    1.1, 0.9, 0.9,
    1, 1.1, 1.2,
    1, 0.975, 0.975
  )
  input_data <- data.frame(ts, site_id, c_id, Site_Event_Normalised_Power_kW, stringsAsFactors = FALSE)
  time_group <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  norm_power <- c(1, 0.95, 0.95)
  ideal_response_downsampled <- data.frame(time_group, norm_power, stringsAsFactors = FALSE)
  out <- calc_error_metric_and_compliance(input_data, ideal_response_downsampled)
  site_id <- c("100", "101", "102", "103")
  abs_percent_diff_actual_cf_ideal <- c(0.03508772, 0.06842105, 0.1403509, 0.01754386)
  percent_diff_actual_cf_ideal <- c(-0.03508772, -0.001754386, 0.1403509, 0.01754386)
  min_diff <- c(-0.05263158, -0.05263158, 0.0, 0.0)
  max_diff <- c(0.0, 0.1, 0.2631579, 0.02631579)
  below_spec <- c(1, 0, 0, 0)
  above_spec <- c(0, 0, 1, 1)
  mixed_wrt_spec <- c(0, 1, 0, 0)
  combined_error_metric <- c(-0.03508772, 0.06842105, 0.1403509, 0.01754386)
  compliance_status <- c("Compliant", "Ambigous", "Non Compliant", "Above Ideal Response")
  expected_output <- data.frame(
    site_id,
    min_diff,
    max_diff,
    abs_percent_diff_actual_cf_ideal,
    percent_diff_actual_cf_ideal,
    mixed_wrt_spec, below_spec, above_spec,
    combined_error_metric,
    compliance_status,
    stringsAsFactors = FALSE
  )
  expected_output <- left_join(input_data, expected_output, by = c("site_id"))
  expect_equal(out, expected_output, tolerance = 0.001)
})


test_that("Test compliance categorisation ", {
  site_id <- c(
    "100", "100", "100", "100",
    "100", "100", "100", "100",
    "101", "101", "101", "101",
    "102", "102", "102", "102",
    "103", "103", "103", "103",
    "104", "104", "104", "104",
    "105", "105", "105", "105",
    "106", "106", "106", "106"
  )
  c_id <- c(
    "1000", "1000", "1000", "1000",
    "1001", "1001", "1001", "1001",
    "1011", "1011", "1011", "1011",
    "1021", "1021", "1021", "1021",
    "1031", "1031", "1031", "1031",
    "1041", "1041", "1041", "1041",
    "1051", "1051", "1051", "1051",
    "1061", "1061", "1061", "1061"
  )
  ts <- c(
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  Site_Event_Normalised_Power_kW <- c(
    1, 0.96, 0.96, 0.96, # Standard good response
    1, 0.96, 0.96, 0.96, # Standard good response
    1, 0.95, 0.95, 1.0, # Good but comes up early
    1, 0.97, 0.97, 0.97, # Doesn't drop far enough
    1, 1, 1, 0.96, # Doesn't come down quick enough
    1, 0.8, 1, 1, # Should end in the Non Compliant Responding cat
    1, 0.8, 1, 1, # Should end up in the dissconect cat
    1, 0.8, 1, 1
  ) # Should end up in the drop to zero cat
  response_category <- c(
    "1", "1", "1", "1",
    "1", "1", "1", "1",
    "2", "2", "2", "2",
    "3", "3", "3", "3",
    "4", "4", "4", "4",
    "6", "6", "6", "6",
    "4 Disconnect", "4 Disconnect", "4 Disconnect", "4 Disconnect",
    "3 Drop to Zero", "3 Drop to Zero", "3 Drop to Zero", "3 Drop to Zero"
  )
  input_data <- data.frame(
    ts,
    site_id,
    c_id,
    Site_Event_Normalised_Power_kW,
    response_category,
    stringsAsFactors = FALSE
  )
  time_group <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  norm_power <- c(1, 0.95, 0.95, 0.95)
  ideal_response_downsampled <- data.frame(time_group, norm_power, stringsAsFactors = FALSE)
  start_buffer <- 60
  end_buffer <- 60
  end_buffer_responding <- 60
  disconnecting_threshold <- 0.05
  threshold <- 0.8
  out <- calc_error_metric_and_compliance_2(
    input_data,
    ideal_response_downsampled,
    threshold,
    start_buffer,
    end_buffer,
    end_buffer_responding,
    disconnecting_threshold
  )
  site_id <- c("100", "101", "102", "103", "104", "105", "106")
  compliance_status <- c(
    "Compliant",
    "Compliant",
    "Non-compliant",
    "Non-compliant",
    "Non-compliant Responding",
    "Disconnect",
    "Drop to Zero"
  )
  expected_output <- data.frame(site_id, compliance_status, stringsAsFactors = FALSE)
  expected_output <- left_join(input_data, expected_output, by = c("site_id"))
  expect_equal(out, expected_output, tolerance = 0.001)
})


test_that("Test the calc of error metrics and categorisation", {
  site_id <- c(
    "100", "100", "100", "100",
    "100", "100", "100", "100",
    "101", "101", "101", "101",
    "102", "102", "102", "102",
    "103", "103", "103", "103",
    "104", "104", "104", "104",
    "105", "105", "105", "105",
    "106", "106", "106", "106"
  )
  c_id <- c(
    "1000", "1000", "1000", "1000",
    "1001", "1001", "1001", "1001",
    "1011", "1011", "1011", "1011",
    "1021", "1021", "1021", "1021",
    "1031", "1031", "1031", "1031",
    "1041", "1041", "1041", "1041",
    "1051", "1051", "1051", "1051",
    "1061", "1061", "1061", "1061"
  )
  ts <- c(
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55",
    "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55"
  )
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  Site_Event_Normalised_Power_kW <- c(
    1, 0.96, 0.96, 0.96, # Standard good response
    1, 0.96, 0.96, 0.96, # Standard good response
    1, 0.95, 0.95, 1.0, # Good but comes up early
    1, 0.97, 0.97, 0.97, # Doesn't drop far enough
    1, 1, 1, 0.96, # Doesn't come down quick enough
    1, 0.8, 1, 1, # Should end in the Non Compliant Responding cat
    1, 0.8, 1, 1, # Should end up in the dissconect cat
    1, 0.8, 1, 1
  ) # Should end up in the drop to zero cat
  response_category <- c(
    "1", "1", "1", "1",
    "1", "1", "1", "1",
    "2", "2", "2", "2",
    "3", "3", "3", "3",
    "4", "4", "4", "4",
    "6", "6", "6", "6",
    "4 Disconnect", "4 Disconnect", "4 Disconnect", "4 Disconnect",
    "3 Drop to Zero", "3 Drop to Zero", "3 Drop to Zero", "3 Drop to Zero"
  )
  input_data <- data.frame(
    ts,
    site_id,
    c_id,
    Site_Event_Normalised_Power_kW,
    response_category,
    stringsAsFactors = FALSE
  )
  time_group <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", "2018-01-01 13:14:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  norm_power <- c(1, 0.95, 0.95, 0.95)
  ideal_response_downsampled <- data.frame(time_group, norm_power, stringsAsFactors = FALSE)
  start_buffer <- 60
  end_buffer <- 60
  end_buffer_responding <- 60
  disconnecting_threshold <- 0.95
  threshold <- 0.8
  out <- calc_error_metric_and_compliance_2(
    input_data,
    ideal_response_downsampled,
    threshold,
    start_buffer,
    end_buffer,
    end_buffer_responding,
    disconnecting_threshold
  )
  site_id <- c("100", "101", "102", "103", "104", "105", "106")
  compliance_status <- c(
    "Compliant",
    "Compliant",
    "Non Compliant",
    "Non-compliant",
    "Non-compliant Responding",
    "Non-compliant Responding",
    "Non-compliant Responding"
  )
  expected_output <- data.frame(site_id, compliance_status, stringsAsFactors = FALSE)
  expected_output <- left_join(input_data, expected_output, by = c("site_id"))
  expect_equal(out, expected_output, tolerance = 0.001)
})
