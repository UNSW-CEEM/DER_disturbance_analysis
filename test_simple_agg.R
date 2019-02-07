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
context("Testing that event anaylsis functions produce comparable aggregate power curves to Naomi's initial analysis")
source("data_manipulation_functions.R")
source("filter_and_aggregate.R")

test_that("2018-08-25 event in SA matches Naomi initial work with pre-cleaning", 
          {
  # Define files to use
  Fault_Datafile <- "test_data/2018-08-25 pre-cleaned inputs/2018-08-25_sa_qld_naomi.feather"
  Circuit_Datafile <- "test_data/2018-08-25 pre-cleaned inputs/circuit_details_TB_V4.csv"
  Site_Datafile <- "test_data/2018-08-25 pre-cleaned inputs/site_details_multiples_summarised_NS_v2.csv"
  
  #Define time of interest
  start_time <- "2018-08-25 10:00:00"
  end_time <- "2018-08-25 16:00:00"
  
  # Load Data
  time_series_data <- read_feather(Fault_Datafile)
  circuit_details <- read.csv(file=Circuit_Datafile, header=TRUE, stringsAsFactors = FALSE) %>% 
    mutate(manual_check_required = ifelse(is.na(manual_check_required), 0, 1))
  site_details <- read.csv(file=Site_Datafile, header=TRUE, stringsAsFactors = FALSE)
  site_details <- process_raw_site_details(site_details)

  # Peform data processing
  combined_data <- combine_data_tables(time_series_data, circuit_details,
                                       site_details)
  combined_data <- filter(combined_data, ts>=start_time & ts<= end_time)
  combined_data <- filter(combined_data, d==60)
  combined_data <- filter(combined_data, s_state=='SA')
  combined_data_c <- filter(combined_data, manual_check_required!=1)
  agg_power <- aggregate(combined_data$power_kW, by=list(Category=combined_data$ts), FUN=sum)
  
  #Load test data from Naomi's work 
  test_data_file <- 'test_data/2018-08-25 test output/2018-08-25 SA Agg Power.csv'
  test_data <- read.csv(file=test_data_file, header=TRUE, stringsAsFactors = FALSE) %>%
    mutate(Time = ymd_hms(Time,tz = "Australia/Brisbane"))
  
  comp <- inner_join(agg_power, test_data, by=c("Category" = "Time")) %>%
    mutate(power_error_in_kW = x - Power_kW) %>%
    mutate(error_percentage = abs(power_error_in_kW/Power_kW))
  
  expect_equal(mean(comp$error_percentage), 0, tolerance=0.012)
})


test_that("2018-08-25 event in SA matches Naomi initial work without pre-cleaning", {
  # Define files to use
  Fault_Datafile <- "test_data/2018-08-25 raw inputs/2018-08-25_sa_qld_naomi.feather"
  Circuit_Datafile <- "test_data/2018-08-25 raw inputs/circuit_details.csv"
  Site_Datafile <- "test_data/2018-08-25 raw inputs/site_details.csv"
  
  #Define time of interest
  start_time <- "2018-08-25 10:00:00"
  end_time <- "2018-08-25 16:00:00"
  
  # Load Data
  time_series_data <- read_feather(Fault_Datafile)
  circuit_details <- read.csv(file=Circuit_Datafile, header=TRUE, stringsAsFactors = FALSE)
  site_details <- read.csv(file=Site_Datafile, header=TRUE, stringsAsFactors = FALSE)
  site_details <- process_raw_site_details(site_details)
  
  # Peform data processing
  combined_data <- combine_data_tables(time_series_data, circuit_details, 
                                       site_details)
  combined_data <- filter(combined_data, ts>=start_time & ts<= end_time)
  combined_data <- filter(combined_data, d==60)
  combined_data <- filter(combined_data, s_state=='SA')
  agg_power <- aggregate(combined_data$power_kW, by=list(Category=combined_data$ts), FUN=sum)
  
  #Load test data from Naomi's work 
  test_data_file <- 'test_data/2018-08-25 test output/2018-08-25 SA Agg Power.csv'
  test_data <- read.csv(file=test_data_file, header=TRUE, stringsAsFactors = FALSE) %>%
    mutate(Time = ymd_hms(Time,tz = "Australia/Brisbane"))
  
  comp <- inner_join(agg_power, test_data, by=c("Category" = "Time")) %>%
    mutate(power_error_in_kW = x - Power_kW) %>%
    mutate(error_percentage = abs(power_error_in_kW/Power_kW))
  
  expect_equal(mean(comp$error_percentage), 0, tolerance=0.012)
})


test_that("2018-08-25 event in SA matches Naomi initial work without 
          pre-cleaning for inverter standard level agg", {
  # Define files to use
  Fault_Datafile <- "test_data/2018-08-25 raw inputs/2018-08-25_sa_qld_naomi.feather"
  Circuit_Datafile <- "test_data/2018-08-25 raw inputs/circuit_details.csv"
  Site_Datafile <- "test_data/2018-08-25 raw inputs/site_details.csv"
  
  #Define time of interest
  start_time <- "2018-08-25 10:00:00"
  end_time <- "2018-08-25 16:00:00"
  
  # Load Data
  time_series_data <- read_feather(Fault_Datafile)
  circuit_details <- read.csv(file=Circuit_Datafile, header=TRUE, stringsAsFactors = FALSE)
  site_details <- read.csv(file=Site_Datafile, header=TRUE, stringsAsFactors = FALSE)
  site_details <- process_raw_site_details(site_details)
  
  # Peform data processing
  combined_data <- combine_data_tables(time_series_data, circuit_details, 
                                       site_details)
  combined_data <- filter(combined_data, ts>=start_time & ts<= end_time)
  combined_data_f <- vector_filter(combined_data, duration=60, 
                                   state="SA", standards="AS4777.3:2005")
  agg_power <- vector_groupby(combined_data_f, agg_on_standard=TRUE)

  #Load test data from Naomi's work 
  test_data_file <- 'test_data/2018-08-25 test output/2018-08-25 SA Agg Power by Standard.csv'
  test_data <- read.csv(file=test_data_file, header=TRUE, stringsAsFactors = FALSE) %>%
    mutate(Time = ymd_hms(Time,tz = "Australia/Brisbane"))
  
  comp <- inner_join(agg_power, test_data, by=c("Time" = "Time")) %>%
    mutate(power_error_in_kW = Power_kW - standard_2005) %>%
    mutate(error_percentage = abs(power_error_in_kW/standard_2005))
  
  expect_equal(mean(comp$error_percentage), 0, tolerance=0.012)
})
