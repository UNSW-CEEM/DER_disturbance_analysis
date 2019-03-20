context("Testing the DER event analysis aggregation functions")
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
source("aggregate_functions.R")

test_that("Test the group by for power with just standard version" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version")
  out <- vector_groupby_power(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_1.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for power with standard version and manufacturer" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version", "manufacturer")
  out <- vector_groupby_power(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_2.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$Time, out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$Time, expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for power with site_id and c_id" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "site_id", "c_id")
  out <- vector_groupby_power(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_3.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$Time, out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$Time, expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for power with just clean" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean")
  out <- vector_groupby_power(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_4.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$Time, out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$Time, expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for v and f with just standard version" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version")
  out <- vector_groupby_f_and_v(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_5.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for v and f with standard version and manufacturer" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version", "manufacturer")
  out <- vector_groupby_f_and_v(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_6.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$Time, out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$Time, expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for v and f with site_id and c_id" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "site_id", "c_id")
  out <- vector_groupby_f_and_v(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_7.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$Time, out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$Time, expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for v and f with just clean" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean")
  out <- vector_groupby_f_and_v(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_8.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$Time, out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$Time, expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for norm power with just standard version" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version")
  out <- vector_groupby_norm_power(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_9.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for norm power with standard version and manufacturer" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version", "manufacturer")
  out <- vector_groupby_norm_power(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_10.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$Time, out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$Time, expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for norm power with site_id and c_id" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "site_id", "c_id")
  out <- vector_groupby_norm_power(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_11.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$Time, out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$Time, expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for norm power with just clean" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean")
  out <- vector_groupby_norm_power(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_12.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$Time, out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$Time, expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for count with just standard version" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version")
  out <- vector_groupby_count(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_13.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for count with standard version and manufacturer" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version", "manufacturer")
  out <- vector_groupby_count(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_14.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$clean, out$Standard_Version, out$manufacturer),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$clean, expected_output$Standard_Version, 
                                           expected_output$manufacturer),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for count with site_id and c_id" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "site_id", "c_id")
  out <- vector_groupby_count(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_15.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$clean, out$site_id),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$clean, expected_output$site_id),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for count with just clean" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean")
  out <- vector_groupby_count(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_16.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$clean),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$clean),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for response count with just standard version" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version")
  out <- vector_groupby_count_response(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_17.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expected_output <- mutate(expected_output, response_category=ifelse(response_category=='NA',NA,response_category))
  out <- out[order(out$series_x, out$series_y),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series_x, expected_output$series_y),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for response count with standard version and manufacturer" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version", "manufacturer")
  out <- vector_groupby_count_response(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_18.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expected_output <- mutate(expected_output, response_category=ifelse(response_category=='NA',NA,response_category))
  out <- out[order(out$series_x, out$series_y),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series_x, expected_output$series_y),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for response count with site_id and c_id" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "site_id", "c_id")
  out <- vector_groupby_count_response(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_19.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expected_output <- mutate(expected_output, response_category=ifelse(response_category=='NA',NA,response_category))
  out <- out[order(out$series_x, out$series_y),]
  rownames(out) <- NULL
  expected_output <- mutate(expected_output, series_y=as.character(series_y))
  expected_output <- expected_output[order(expected_output$series_x, expected_output$series_y),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for response count with just clean" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean")
  out <- vector_groupby_count_response(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_20.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expected_output <- mutate(expected_output, response_category=ifelse(response_category=='NA',NA,response_category))
  out <- out[order(out$series_x, out$series_y),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series_x, expected_output$series_y),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for zone count with just standard version" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version")
  out <- vector_groupby_count_zones(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_21.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expected_output <- mutate(expected_output, zone=ifelse(zone=='NA',NA,zone))
  out <- out[order(out$series_x, out$series_y),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series_x, expected_output$series_y),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for zone count with standard version and manufacturer" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version", "manufacturer")
  out <- vector_groupby_count_zones(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_22.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expected_output <- mutate(expected_output, zone=ifelse(zone=='NA',NA,zone))
  out <- out[order(out$series_x, out$series_y),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series_x, expected_output$series_y),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for zone count with site_id and c_id" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "site_id", "c_id")
  out <- vector_groupby_count_zones(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_23.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expected_output <- mutate(expected_output, zone=ifelse(zone=='NA',NA,zone))
  out <- out[order(out$series_x, out$series_y),]
  rownames(out) <- NULL
  expected_output <- mutate(expected_output, series_y=as.character(series_y))
  expected_output <- expected_output[order(expected_output$series_x, expected_output$series_y),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for zone count with just clean" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean")
  out <- vector_groupby_count_zones(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_24.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  expected_output <- mutate(expected_output, zone=ifelse(zone=='NA',NA,zone))
  out <- out[order(out$series_x, out$series_y),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series_x, expected_output$series_y),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for distance response with just standard version" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version")
  out <- vector_groupby_cumulative_distance(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_25.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for distance response with standard version and manufacturer" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version", "manufacturer")
  out <- vector_groupby_cumulative_distance(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_26.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for distance response with site_id and c_id" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "site_id", "c_id")
  out <- vector_groupby_cumulative_distance(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_27.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for distance response with just clean" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean")
  out <- vector_groupby_cumulative_distance(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_28.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$series),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$series),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for geo data with just standard version" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version")
  out <- vector_groupby_system(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_29.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$site_id),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$site_id),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for geo data with standard version and manufacturer" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "Standard_Version", "manufacturer")
  out <- vector_groupby_system(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_30.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$site_id),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$site_id),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for geo data with site_id and c_id" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean", "site_id", "c_id")
  out <- vector_groupby_system(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_31.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$site_id),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$site_id),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test the group by for geo data with just clean" ,{
  input <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_agg_data.csv"
  input <- read.csv(file=input, header=TRUE, stringsAsFactors = FALSE)
  group_cols <- c("clean")
  out <- vector_groupby_system(input, group_cols)
  expected_output <- "C:/Users/user/Documents/GitHub/DER_disturbance_analysis/auto_test_data/test_out_32.csv"
  expected_output <- read.csv(file=expected_output, header=TRUE, stringsAsFactors = FALSE)
  out <- out[order(out$site_id),]
  rownames(out) <- NULL
  expected_output <- expected_output[order(expected_output$site_id),]
  rownames(expected_output) <- NULL
  expect_equal(out, expected_output, tolerance=0.001)
})