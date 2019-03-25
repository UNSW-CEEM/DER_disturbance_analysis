context("Testing the data cleaning functions functions")
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
source("data_cleaning_functions.R")

test_that("Test calculation of sunrise and sunset times",{
  # Test input data
  postcode <- c("800", "4064", "6019")		
  lat <- c(-12.462, -27.463, -31.907)
  lon <- c(130.844, 152.942, 115.771)
  date1 <- ymd("2018-01-02")
  postcode_data <- data.frame(postcode, lon, lat, stringsAsFactors = FALSE)
  # Test output data
  postcode <- c("800", "4064", "6019")		
  lat <- c(-12.462, -27.463, -31.907)
  lon <- c(130.844, 152.942, 115.771)
  dis_sunrise <- c("2018-01-02 05:56:20", "2018-01-02 03:58:04", "2018-01-02 06:16:17")
  sunrise <- strptime(dis_sunrise, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")
  dis_sunset <- c("2018-01-02 20:46:45", "2018-01-02 19:48:12", "2018-01-02 22:27:26")
  sunset <- strptime(dis_sunset, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")
  date <- c(ymd("2018-01-02"), ymd("2018-01-02"), ymd("2018-01-02"))
  expected_out <- data.frame(postcode, lon, lat, date, sunrise, sunset, stringsAsFactors = FALSE)
  # Call processing function
  out = calc_sunrise_sunset_bounds(postcode_data, date1)
  # Test the answer matches the expected answer
  expect_equal(select(out, postcode, lon, lat, date, sunrise, sunset), expected_out, tolerance=0.01)
})

test_that("Test the calculation of summary values for circuit info cleaning" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_cleaning_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  input$dis_sunrise <- "2018-08-25 06:00:00"
  input$dis_sunset <- "2018-08-25 18:00:00"
  input$sunrise <- fastPOSIXct("2018-08-25 06:00:00")
  input$sunset <- fastPOSIXct("2018-08-25 18:00:00")
  input <- input %>%  mutate(ts = fastPOSIXct(ts))
  out <- clac_output_summary_values(input)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_c_id_cleaning.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$c_id),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$c_id),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test checking of day vs night energy",{
  # Test input data
  c_id <- c("800", "4064", "6019", "6018", "1000")
  con_type <- c("pv_site", "pv_site_net", "pv_inverter_net", "pv_inverter_net", "pv_inverter_net")
  energy_day <- c(5, 10, 1, 10, 1)
  energy_night <- c(5, 1, 10, 1, 10)
  first_ac <- c(1, 1, 1, 1, 100)
  energy_data <- data.frame(c_id, con_type, energy_day, energy_night, first_ac, stringsAsFactors = FALSE)
  energy_data <- mutate(energy_data, frac_day=energy_day/(energy_day+energy_night))
  # Test output data
  c_id <- c("800", "4064", "6019", "6018", "1000")
  con_type <- c("load", "pv_site_net", "load", "pv_inverter_net", "pv_inverter_net")
  energy_day <- c(5, 10, 1, 10, 1)
  energy_night <- c(5, 1, 10, 1, 10)
  first_ac <- c(1, 1, 1, 1, 100)
  expected_out <- data.frame(c_id, con_type, energy_day, energy_night, first_ac, stringsAsFactors = FALSE)
  expected_out <- mutate(expected_out, frac_day=energy_day/(energy_day+energy_night))
  # Call processing function
  out = check_day_vs_night_energy(energy_data)
  # Test the answer matches the expected answer
  expect_equal(out, expected_out, tolerance=0.01)
})

test_that("Test checking of reversed ploarity",{
  # Test input data
  c_id <- c("800", "4064", "6019", "6018", "1000")
  con_type <- c("pv_site", "pv_site_net", "pv_inverter_net", "load", "pv_inverter_net")
  energy_day <- c(0.13, 0.12, 0.13, 0.13, 0.13)
  energy_night <- c(0.12, 0.12, 0.12, 0.12, 0.12)
  min_power <- c(-6, -6, 6, -6, -6)
  max_power <- c(5, 5, 5, 5, 5)
  first_ac <- c(1, 1, 1, 1, 1)
  polarity <- c(1, 1, 1, 1, -1)
  energy_data <- data.frame(c_id, con_type, energy_day, energy_night, first_ac, min_power, max_power, polarity, 
                            stringsAsFactors = FALSE)
  # Test output data
  c_id <- c("800", "4064", "6019", "6018", "1000")
  con_type <- c("pv_site", "pv_site_net", "pv_inverter_net", "load", "pv_inverter_net")
  energy_day <- c(0.13, 0.12, 0.13, 0.13, 0.13)
  energy_night <- c(0.12, 0.12, 0.12, 0.12, 0.12)
  min_power <- c(-6, -6, 6, -6, -6)
  max_power <- c(5, 5, 5, 5, 5)
  first_ac <- c(1, 1, 1, 1, 1)
  polarity <- c(-1, 1, 1, 1, 1)
  expected_out <- data.frame(c_id, con_type, energy_day, energy_night, first_ac, min_power, max_power, polarity,
                             stringsAsFactors = FALSE)
  # Call processing function
  out = check_for_reversed_polarity(energy_data)
  # Test the answer matches the expected answer
  expect_equal(out, expected_out, tolerance=0.01)
})

test_that("Test checking of mixed ploarity",{
  # Test input data
  c_id <- c("800", "4064", "6019", "6018", "1000")
  con_type <- c("pv_site", "pv_site_net", "pv_inverter_net", "load", "pv_inverter_net")
  min_power <- c(-6, -0.11, -0.1, -0.11, 0.11)
  max_power <- c(5, 0.11, 0.1, -0.11, 0.11)
  first_ac <- c(1, 1, 1, 1, 1)
  energy_day <- c(0.13, 0.12, 0.13, 0.13, 0.13)
  energy_night <- c(0.12, 0.12, 0.12, 0.12, 0.12)
  energy_data <- data.frame(c_id, con_type, first_ac, min_power, max_power,
                            stringsAsFactors = FALSE)
  # Test output data
  c_id <- c("800", "4064", "6019", "6018", "1000")
  con_type <- c("mixed", "mixed", "pv_inverter_net", "load", "pv_inverter_net")
  min_power <- c(-6, -0.11, -0.1, -0.11, 0.11)
  max_power <- c(5, 0.11, 0.1, -0.11, 0.11)
  first_ac <- c(1, 1, 1, 1, 1)
  energy_day <- c(0.13, 0.12, 0.13, 0.13, 0.13)
  energy_night <- c(0.12, 0.12, 0.12, 0.12, 0.12)
  expected_out <- data.frame(c_id, con_type, first_ac, min_power, max_power,
                             stringsAsFactors = FALSE)
  # Call processing function
  out = check_for_mixed_polarity(energy_data)
  # Test the answer matches the expected answer
  expect_equal(out, expected_out, tolerance=0.01)
})