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

