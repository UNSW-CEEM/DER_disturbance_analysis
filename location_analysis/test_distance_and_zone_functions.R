context("Testing the DER event analysis distance and zone functions")
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
library(geosphere)
source("distance_from_event.R")

test_that("Test the calculation of the distance between two sets of latitude and longditude" ,{
  # Test input data
  postcode <- c(2011, 3012, 4001)
  lat <- c(-33.132382, -25.044100, -28.135162)
  lon <- c(150.985190, 146.096544, 121.293364)
  event_lat <- -33.060483
  event_lon <- 135.975562
  input <- data.frame(postcode, lat, lon)
  out <- get_postcode_distance_from_event(input, event_lat, event_lon)
  postcode <- c(2011, 3012, 4001)
  lat <- c(-33.132382, -25.044100, -28.135162)
  lon <- c(150.985190, 146.096544, 121.293364)
  distance <- c(1397, 1326, 1506)
  expected_output <- data.frame(postcode, distance, lat, lon, stringsAsFactors=FALSE)
  expect_equal(out, expected_output, tolerance=0.01)
})

test_that("Test the setting og zones based on a distance" ,{
  # Test input data
  z1 <- 300
  z2 <- 600
  z3 <- 1000
  postcode <- c(2011, 3012, 4001, 8)
  distance <- c(200, 500, 800, 2000)
  input <- data.frame(postcode, distance)
  output <- get_zones(input, z1, z2, z3)
  z1 <- 300
  z2 <- 600
  z3 <- 1000
  postcode <- c(2011, 3012, 4001, 8)
  distance <- c(200, 500, 800, 2000)
  zone <- c("1 Zone", "2 Zone", "3 Zone", "4 Undefined")
  expected_output <- data.frame(postcode, distance, zone, stringsAsFactors=FALSE)
  expect_equal(output, expected_output, tolerance=0.01)
})