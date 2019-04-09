library(lubridate)
library(dplyr)
library(assertthat)
library(testthat)
source("ideal_response_functions.R")
context("Testing the DER event analysis distance and zone functions")

test_that("Test the calculation of the ideal inverter response to over frequency, simple case" ,{
  ts <- c("2018-01-01 13:11:50", "2018-01-01 13:11:51", "2018-01-01 13:11:52", "2018-01-01 13:11:53", 
          "2018-01-01 13:11:54", "2018-01-01 13:11:55", "2018-01-01 13:11:56", "2018-01-01 13:11:57")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(50.0, 50.18, 50.25, 50.3, 51.5, 50.5, 50.5, 50.5)
  input_data <- data.frame(ts, f, stringsAsFactors = FALSE)
  out <- ideal_response(input_data)
  ts <- c("2018-01-01 13:11:52", "2018-01-01 13:11:53", "2018-01-01 13:11:54", "2018-01-01 13:11:55", 
          "2018-01-01 13:11:56", "2018-01-01 13:11:57")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(50.25, 50.3, 51.5, 50.5, 50.5, 50.5)
  norm_power <- c(1.0, 0.97142857142, 0.28571428571, 0.28571428571, 0.28571428571, 0.28571428571)
  expected_output <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the calculation of the ideal inverter response to over frequency, don't go to zero for just one reading 
           of f=52" ,{
  ts <- c("2018-01-01 13:11:50", "2018-01-01 13:11:51", "2018-01-01 13:11:52", "2018-01-01 13:11:53", 
          "2018-01-01 13:11:54", "2018-01-01 13:11:55", "2018-01-01 13:11:56", "2018-01-01 13:11:57")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(50.0, 50.0, 52.0, 50.3, 51.5, 50.5, 50.5, 50.5)
  input_data <- data.frame(ts, f, stringsAsFactors = FALSE)
  out <- ideal_response(input_data)
  ts <- c("2018-01-01 13:11:52", "2018-01-01 13:11:53", "2018-01-01 13:11:54", "2018-01-01 13:11:55", 
          "2018-01-01 13:11:56", "2018-01-01 13:11:57")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(52.0, 50.3, 51.5, 50.5, 50.5, 50.5)
  norm_power <- c(1.0, 0.97142857142, 0.28571428571, 0.28571428571, 0.28571428571, 0.28571428571)
  expected_output <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the calculation of the ideal inverter response to over frequency, go to zero for just 2 readings 
           of f=52" ,{
  ts <- c("2018-01-01 13:11:50", "2018-01-01 13:11:51", "2018-01-01 13:11:52", "2018-01-01 13:11:53", 
         "2018-01-01 13:11:54", "2018-01-01 13:11:55", "2018-01-01 13:11:56", "2018-01-01 13:11:57")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(50.0, 50.0, 52.0, 52.0, 51.5, 50.5, 50.5, 50.5)
  input_data <- data.frame(ts, f, stringsAsFactors = FALSE)
  out <- ideal_response(input_data)
  ts <- c("2018-01-01 13:11:52", "2018-01-01 13:11:53", "2018-01-01 13:11:54", "2018-01-01 13:11:55", 
         "2018-01-01 13:11:56", "2018-01-01 13:11:57")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(52.0, 52.0, 51.5, 50.5, 50.5, 50.5)
  norm_power <- c(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  expected_output <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the calculation of the ideal inverter response to over frequency, no response defined after 60s past
          return to normal frequency" ,{
  ts <- c("2018-01-01 13:11:50", "2018-01-01 13:11:51", "2018-01-01 13:11:52", "2018-01-01 13:12:51", 
         "2018-01-01 13:12:52", "2018-01-01 13:12:53", "2018-01-01 13:12:54", "2018-01-01 13:12:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08, 50.05, 50.05, 50.05, 50.05)
  input_data <- data.frame(ts, f, stringsAsFactors = FALSE)
  out <- ideal_response(input_data)
  ts <- c("2018-01-01 13:11:50", "2018-01-01 13:11:51", "2018-01-01 13:11:52", "2018-01-01 13:12:51")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08)
  norm_power <- c(1.0, 0.0, 0.0, 0.0)
  expected_output <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the downsampling of the 1 s Ideal response, simple" ,{
  ts <- c("2018-01-01 13:10:56", "2018-01-01 13:11:51", "2018-01-01 13:11:55", "2018-01-01 13:11:56", 
          "2018-01-01 13:12:52", "2018-01-01 13:12:53", "2018-01-01 13:12:54", "2018-01-01 13:12:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08, 50.05, 50.05, 50.05, 50.05)
  norm_power <- c(152.0, 152.0, 150.15, 150.08, 155.05, 157.05, 159.05, 151.05)
  input_data <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  out <- down_sample_1s(input_data, 60, as.POSIXct(strptime("2018-01-01 13:11:55", "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")))
  time_group <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(50.15, 50.05)
  norm_power <- c(151.3833, 154.456)
  expected_output <- data.frame(time_group, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the downsampling of the 1 s Ideal response, last minute with incomplete data" ,{
  ts <- c("2018-01-01 13:10:56", "2018-01-01 13:11:51", "2018-01-01 13:11:55", "2018-01-01 13:12:51", 
          "2018-01-01 13:12:52", "2018-01-01 13:12:53", "2018-01-01 13:12:54", "2018-01-01 13:12:54")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08, 50.05, 50.05, 50.05, 50.05)
  norm_power <- c(152.0, 152.0, 150.15, 150.08, 155.05, 157.05, 159.05, 151.05)
  input_data <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  out <- down_sample_1s(input_data, 60, as.POSIXct(strptime("2018-01-01 13:11:55", "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")))
  time_group <- c("2018-01-01 13:11:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(50.15)
  norm_power <- c(151.3833)
  expected_output <- data.frame(time_group, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the downsampling of the 1 s Ideal response, first minute with incomplete data" ,{
  ts <- c("2018-01-01 13:11:50", "2018-01-01 13:11:51", "2018-01-01 13:11:55", "2018-01-01 13:11:56", 
          "2018-01-01 13:12:52", "2018-01-01 13:12:53", "2018-01-01 13:12:54", "2018-01-01 13:12:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(52.0, 52.0, 50.15, 50.08, 50.05, 50.05, 50.05, 50.05)
  norm_power <- c(152.0, 152.0, 150.15, 150.08, 155.05, 157.05, 159.05, 151.05)
  input_data <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  out <- down_sample_1s(input_data, 60, as.POSIXct(strptime("2018-01-01 13:11:55", "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")))
  time_group <- c("2018-01-01 13:12:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  f <- c(50.05)
  norm_power <- c(154.456)
  expected_output <- data.frame(time_group, f, norm_power, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the calculation of the error metric on a site basis" ,{
  site_id <- c("100", "100", "100",
               "100", "100", "100", 
               "101", "101", "101", 
               "102", "102", "102")
  c_id <- c("1000", "1000", "1000", 
            "1001", "1001", "1001", 
            "1011", "1011", "1011", 
            "1021", "1021", "1021")
  ts <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", 
          "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55",
          "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55", 
          "2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  Site_Event_Normalised_Power_kW <- c(1, 0.9, 0.9,
                                      1, 0.9, 0.9,
                                      1.1, 0.9, 0.9,
                                      1, 1.1, 1.2)
  input_data <- data.frame(ts, site_id, c_id,  Site_Event_Normalised_Power_kW, stringsAsFactors=FALSE)
  time_group <- c("2018-01-01 13:11:55", "2018-01-01 13:12:55", "2018-01-01 13:13:55")
  time_group <- as.POSIXct(strptime(time_group, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  norm_power <- c(1, 0.95, 0.95)
  ideal_response_downsampled <- data.frame(time_group, norm_power, stringsAsFactors=FALSE)
  out <- calc_error_metric(input_data, ideal_response_downsampled)
  site_id <- c("100", "101", "102")
  abs_percent_diff_actual_cf_ideal <- c(0.03508772, 0.06842105, 0.1403509)
  percent_diff_actual_cf_ideal <- c(-0.03508772,  -0.001754386, 0.1403509)
  min_diff <- c(-0.05263158, -0.05263158, 0.0)
  max_diff <- c(0.0, 0.1, 0.2631579)
  below_spec <- c(1, 0, 0)
  above_spec <- c(0, 0, 1)
  mixed_wrt_spec <- c(0, 1, 0)
  combined_error_metric <- c(-0.03508772, 0.06842105, 0.1403509)
  expected_output <- data.frame(site_id,min_diff, max_diff,  abs_percent_diff_actual_cf_ideal,
                                percent_diff_actual_cf_ideal,
                                mixed_wrt_spec, below_spec, above_spec, combined_error_metric,
                                stringsAsFactors=FALSE)
  out <- out[order(out$site_id),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$site_id),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})


