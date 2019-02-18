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

test_that("Test assertion of performance data assumptions, no duplicates on
          c_id and ts",{
          # Test input data
          c_id <- c("101", "101")
          ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00")
          site_id <- c("1", "1")
          clean <- c("cleaned", "cleaned")
          power_kW <- c(58, 67)
          sum_ac <- c(70, 80)
          s_state <- c("NSW", "NSW")
          Standard_Version <- c("Transition", "Transition")
          Grouping <- c("30-100KW", "30-100KW")
          performance_data <- data.frame(ts, c_id, site_id, clean, power_kW, sum_ac, s_state,
                                         Standard_Version, Grouping, stringsAsFactors = FALSE)
          s_state <- c("NSW")
          Standard_Version <- c("Transition")
          Grouping <- c("30-100KW")
          date <- c("2017-12-01")
          standard_capacity <- c(500)
          install_data <- data.frame(s_state, Standard_Version, Grouping, date, standard_capacity, stringsAsFactors = FALSE)
          # Test output data
          expect_error(upscale(performance_data, install_data))
        })

test_that("Test assertion of performance data assumptions, power data is not NaN",{
            # Test input data
            c_id <- c("101", "102")
            ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00")
            site_id <- c("1", "1")
            clean <- c("cleaned", "cleaned")
            power_kW <- c((0/0), 67)
            sum_ac <- c(70, 80)
            s_state <- c("NSW", "NSW")
            Standard_Version <- c("Transition", "Transition")
            Grouping <- c("30-100KW", "30-100KW")
            performance_data <- data.frame(ts, c_id, site_id, clean, power_kW, sum_ac, s_state,
                                           Standard_Version, Grouping, stringsAsFactors = FALSE)
            s_state <- c("NSW")
            Standard_Version <- c("Transition")
            Grouping <- c("30-100KW")
            date <- c("2017-12-01")
            standard_capacity <- c(500)
            install_data <- data.frame(s_state, Standard_Version, Grouping, date, standard_capacity, stringsAsFactors = FALSE)
            # Test output data
            expect_error(upscale(performance_data, install_data))
          })

test_that("Test assertion of performance data assumptions, power data is numeric"
          ,{
            # Test input data
            c_id <- c("101", "102")
            ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00")
            site_id <- c("1", "1")
            clean <- c("cleaned", "cleaned")
            power_kW <- c("x", 67)
            sum_ac <- c(70, 80)
            s_state <- c("NSW", "NSW")
            Standard_Version <- c("Transition", "Transition")
            Grouping <- c("30-100KW", "30-100KW")
            performance_data <- data.frame(ts, c_id, site_id, clean, power_kW, sum_ac, s_state,
                                           Standard_Version, Grouping, stringsAsFactors = FALSE)
            s_state <- c("NSW")
            Standard_Version <- c("Transition")
            Grouping <- c("30-100KW")
            date <- c("2017-12-01")
            standard_capacity <- c(500)
            install_data <- data.frame(s_state, Standard_Version, Grouping, date, standard_capacity, stringsAsFactors = FALSE)
            # Test output data
            expect_error(upscale(performance_data, install_data))
          })

test_that("Test assertion of performance data assumptions, ac data is not NaN",{
            # Test input data
            c_id <- c("101", "102")
            ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00")
            site_id <- c("1", "1")
            clean <- c("cleaned", "cleaned")
            power_kW <- c(70, 67)
            sum_ac <- c("y", 80)
            s_state <- c("NSW", "NSW")
            Standard_Version <- c("Transition", "Transition")
            Grouping <- c("30-100KW", "30-100KW")
            performance_data <- data.frame(ts, c_id, site_id, clean, power_kW, sum_ac, s_state,
                                           Standard_Version, Grouping, stringsAsFactors = FALSE)
            s_state <- c("NSW")
            Standard_Version <- c("Transition")
            Grouping <- c("30-100KW")
            date <- c("2017-12-01")
            standard_capacity <- c(500)
            install_data <- data.frame(s_state, Standard_Version, Grouping, date, standard_capacity, stringsAsFactors = FALSE)
            # Test output data
            expect_error(upscale(performance_data, install_data))
          })

test_that("Test assertion of performance data assumptions, ac data is numeric",{
            # Test input data
            c_id <- c("101", "102")
            ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00")
            site_id <- c("1", "1")
            clean <- c("cleaned", "cleaned")
            power_kW <- c(70, 67)
            sum_ac <- c((0/0), 80)
            s_state <- c("NSW", "NSW")
            Standard_Version <- c("Transition", "Transition")
            Grouping <- c("30-100KW", "30-100KW")
            performance_data <- data.frame(ts, c_id, site_id, clean, power_kW, sum_ac, s_state,
                                           Standard_Version, Grouping, stringsAsFactors = FALSE)
            s_state <- c("NSW")
            Standard_Version <- c("Transition")
            Grouping <- c("30-100KW")
            date <- c("2017-12-01")
            standard_capacity <- c(500)
            install_data <- data.frame(s_state, Standard_Version, Grouping, date, standard_capacity, stringsAsFactors = FALSE)
            # Test output data
            expect_error(upscale(performance_data, install_data))
          })

test_that("Test assertion of performance data assumptions, ac data is greater 
          than zero" ,{
            # Test input data
            c_id <- c("101", "102")
            ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00")
            site_id <- c("1", "1")
            clean <- c("cleaned", "cleaned")
            power_kW <- c(70, 67)
            sum_ac <- c(0, 80)
            s_state <- c("NSW", "NSW")
            Standard_Version <- c("Transition", "Transition")
            Grouping <- c("30-100KW", "30-100KW")
            performance_data <- data.frame(ts, c_id, site_id, clean, power_kW, sum_ac, s_state,
                                           Standard_Version, Grouping, stringsAsFactors = FALSE)
            s_state <- c("NSW")
            Standard_Version <- c("Transition")
            Grouping <- c("30-100KW")
            date <- c("2017-12-01")
            standard_capacity <- c(500)
            install_data <- data.frame(s_state, Standard_Version, Grouping, date, standard_capacity, stringsAsFactors = FALSE)
            # Test output data
            expect_error(upscale(performance_data, install_data))
          })

test_that("Test a simple upscaling example" ,{
            # Test input data
            c_id <- c("101", "102", "103", "104", "105", "106",
                      "101", "102", "103", "104", "105", "106")
            ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00",
                    "2018-01-01 00:01:00", "2018-01-01 00:01:00",
                    "2018-01-01 00:01:00", "2018-01-01 00:01:00",
                    "2018-01-01 00:02:00", "2018-01-01 00:02:00",
                    "2018-01-01 00:02:00", "2018-01-01 00:02:00",
                    "2018-01-01 00:02:00", "2018-01-01 00:02:00")
            ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
            site_id <- c("1", "1", "2", "3", "4", "6",
                         "1", "1", "2", "3", "4", "6")
            clean <- c("cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned",
                       "cleaned", "cleaned", "cleaned", "cleaned", "cleaned", "cleaned")
            power_kW <- c(70, 67, 12, 50.0, 99, 8, 71, 68, 13, 50.5, 100, 7)
            sum_ac <- c(190, 190, 15, 50, 120, 10, 190, 190, 15, 50, 120, 10)
            s_state <- c("NSW", "NSW", "NSW", "NSW", "NSW", "NSW",
                         "NSW", "NSW", "NSW", "NSW", "NSW", "NSW")
            Standard_Version <- c("Transition", "Transition", "Transition", "Transition",
                                  "AS4777.3:2005", "AS4777.3:2005","Transition", 
                                  "Transition", "Transition", "Transition",
                                  "AS4777.3:2005", "AS4777.3:2005")
            Grouping <- c("30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW",
                          "30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW")
            performance_data <- data.frame(ts, c_id, site_id, clean, power_kW, sum_ac, s_state,
                                           Standard_Version, Grouping, stringsAsFactors = FALSE)
            s_state <- c("NSW", "NSW", "NSW", "NSW", "NSW", "NSW")
            Standard_Version <- c("Transition", "Transition","Transition", "AS4777.3:2005",
                                  "AS4777.3:2005", "AS4777.3:2005")
            Grouping <- c("30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW", "30-100KW")
            date <- c("2017-12-01", "2017-12-02", "2019-01-02","2017-12-01", "2017-12-02", "2019-01-02")
            date <- ymd(date)
            standard_capacity <- c(600, 700, 700, 1200, 1200, 1300)
            install_data <- data.frame(s_state, Standard_Version, Grouping, date, standard_capacity, stringsAsFactors = FALSE)
            # Test output data
            ts <- c("2018-01-01 00:01:00", "2018-01-01 00:01:00",
                    "2018-01-01 00:02:00", "2018-01-01 00:02:00")
            ts <- as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane"))
            Standard_Version <- c("AS4777.3:2005", "Transition", "AS4777.3:2005", "Transition")
            Grouping <- c("30-100KW", "30-100KW", "30-100KW", "30-100KW")
            clean <- c("cleaned", "cleaned", "cleaned", "cleaned")
            performance_factor <- c(0.81250000000000000, 0.84035087719298300, 
                                    0.76666666666666700, 0.86941520467836300)
            power_kW <- c(975.0000000000000, 588.2456140350880, 920.0000000000000, 
                          608.5906432748540)
            standard_capacity <- c(1200, 700, 1200, 700)
            date <- c("2017-12-02", "2017-12-02", "2017-12-02", "2017-12-02")
            date <- ymd(date)
            s_state <- c("NSW", "NSW", "NSW", "NSW")
            expected_output <- data.frame(ts, s_state, Standard_Version, 
                                          Grouping, clean, performance_factor,
                                          standard_capacity, power_kW,
                                          stringsAsFactors = FALSE)
            out <- upscale(performance_data, install_data)
            expect_equal(out, expected_output)
          })
