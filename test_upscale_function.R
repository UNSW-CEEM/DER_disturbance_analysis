context("Testing the DER event analysis upscale function")
##RSHINY APP
library (shiny)
library(shinyTime)
library(shinyWidgets)
library(shinyalert)
library(plotly)
library(feather)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(shinycssloaders)
library(shinyFiles)
library(shinyjs)
library(stringr)
library(fasttime)
library(DT)
library(suncalc)
library(ggmap)
library(measurements)
library(assertthat)
source("upscale_function.R")

test_that("Test a simple upscaling example" ,{
            # Test input data
            ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00", "2018-01-01 00:01:00", "2018-01-01 00:01:00", 
                    "2018-01-01 00:01:00", "2018-01-01 00:02:00", "2018-01-01 00:02:00", "2018-01-01 00:02:00",
                    "2018-01-01 00:02:00", "2018-01-01 00:02:00")
            ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
            site_id <- c("1", "2", "3", "4", "6", "1", "2", "3", "4", "6")
            clean <- c( "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", 
                        "cleaned", "cleaned")
            power_kW <- c(70, 12, 50.0, 99, 8, 71, 68, 50.5, 100, 7)
            sum_ac <- c(190, 15, 50, 120, 10, 190, 15, 50, 120, 10)
            site_performance_factor <- c(70/190, 12/15, 50/50, 99/120, 8/10, 71/190, 68/15, 50.5/50, 100/120, 7/10)
            s_state <- c("NSW", "NSW", "NSW", "NSW", "NSW", "NSW", "NSW", "NSW", "NSW", "NSW")
            Standard_Version <- c("Transition", "Transition", "Transition", "AS4777.3:2005", "AS4777.3:2005",
                                  "Transition", "Transition", "Transition", "AS4777.3:2005", "AS4777.3:2005")
            Grouping <- c("30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW", 
                          "30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW")
            performance_data <- data.frame(ts, site_id, clean, power_kW, sum_ac, site_performance_factor, s_state,
                                           Standard_Version, Grouping, stringsAsFactors = FALSE)
            s_state <- c("NSW", "NSW", "NSW", "NSW", "NSW", "NSW")
            Standard_Version <- c("Transition", "Transition","Transition", "AS4777.3:2005",
                                  "AS4777.3:2005", "AS4777.3:2005")
            Grouping <- c("30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW")
            date <- c("2017-12-02", "2019-01-02","2017-12-01", "2017-12-01", "2017-12-02", "2019-01-02")
            date <- ymd(date)
            standard_capacity <- c(700, 700, 600, 1200, 1200, 1300)
            install_data <- data.frame(s_state, Standard_Version, Grouping, date, standard_capacity, 
                                       stringsAsFactors = FALSE)
            # Test output data
            ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00", "2018-01-01 00:02:00", "2018-01-01 00:02:00")
            ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
            Standard_Version <- c("AS4777.3:2005", "Transition", "AS4777.3:2005", "Transition")
            Grouping <- c("30-100KW", "30-100KW", "30-100KW", "30-100KW")
            clean <- c("cleaned", "cleaned", "cleaned", "cleaned")
            performance_factor <- c( 0.8125, 0.722807, 0.7666667, 1.972339)
            power_kW <- c( 975, 505.9649, 920, 1380.637)
            standard_capacity <- c(1200, 700, 1200, 700)
            date <- c("2017-12-02", "2017-12-02", "2017-12-02", "2017-12-02")
            date <- ymd(date)
            s_state <- c("NSW", "NSW", "NSW", "NSW")
            expected_output <- data.frame(ts, s_state, Standard_Version, Grouping, clean, performance_factor,
                                          standard_capacity, power_kW, stringsAsFactors = FALSE)
            out <- upscale(performance_data, install_data)
            expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test calculation of site performance factors" ,{
  # Test input data
  ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00", "2018-01-01 00:01:00", "2018-01-01 00:01:00", 
          "2018-01-01 00:02:00", "2018-01-01 00:02:00", "2018-01-01 00:02:00", "2018-01-01 00:02:00")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  site_id <- c("1", "2", "2", "3", "1", "2", "2", "3")
  clean <- c( "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned")
  power_kW <- c(70, 12, 50.0, 99, 8, 71, 68, 50.5)
  sum_ac <- c(190, 15, 15, 120, 190, 15, 15, 120)
  performance_data <- data.frame(ts, site_id, clean, power_kW, sum_ac, stringsAsFactors = FALSE)
  # Test output data
  ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00", "2018-01-01 00:01:00", "2018-01-01 00:01:00", 
          "2018-01-01 00:02:00", "2018-01-01 00:02:00", "2018-01-01 00:02:00", "2018-01-01 00:02:00")
  ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
  site_id <- c("1", "2", "2", "3", "1", "2", "2", "3")
  clean <- c( "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned")
  power_kW <- c(70, 12, 50.0, 99, 8, 71, 68, 50.5)
  sum_ac <- c(190, 15, 15, 120, 190, 15, 15, 120)
  site_performance_factor = c(70/190, 62/15, 62/15, 99/120, 8/190, 139/15, 139/15, 50.5/120)
  expected_output <- data.frame(ts, site_id, clean, power_kW, sum_ac, site_performance_factor, 
                                 stringsAsFactors = FALSE)
  out <- calc_site_performance_factors(performance_data)
  expect_equal(out, expected_output, tolerance=0.001)
})