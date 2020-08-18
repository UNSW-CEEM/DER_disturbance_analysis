
testthat::context("Testing calculation of reconnection time.")


load_test_df <- function(text){
  df <- read.table(text = gsub(" ", "", text), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Calculating reconnection times works",{
  
  event_time <- as.POSIXct("2018-01-01 00:01:00", tz = "Australia/Brisbane")
  
  disconnect_threshold <- 0.05
  
  reconnect_threshold <- 0.95
  
  normalised_power <- "                 ts, c_id, c_id_norm_power
                       2018-01-01 00:01:00,    1,           0.00
                       2018-01-01 00:02:00,    1,           0.04
                       2018-01-01 00:03:00,    1,           0.99
                       2018-01-01 00:04:00,    1,           1.00
                       2018-01-01 00:01:00,    2,           0.00
                       2018-01-01 00:02:00,    2,           0.04
                       2018-01-01 00:03:00,    2,           0.60
                       2018-01-01 00:04:00,    2,           0.97
                       2018-01-01 00:01:00,    3,           0.99
                       2018-01-01 00:02:00,    3,           0.01
                       2018-01-01 00:03:00,    3,           0.03
                       2018-01-01 00:04:00,    3,           0.70
                       2018-01-01 00:05:00,    3,           0.75
                       2018-01-01 00:05:00,    4,           0.50"
  

  
  normalised_power <- load_test_df(normalised_power)
  normalised_power <- dplyr::mutate(normalised_power, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))
  
  
  expected_reconnection_times <- "c_id, reconnection_time
                                     1,               1.0
                                     2,               2.0"
  
  expected_reconnection_times <- load_test_df(expected_reconnection_times)
  
  calculated_reconnection_times <- calculate_reconnection_times(normalised_power, event_time, 
                                                                disconnect_threshold = 0.05, reconnect_threshold = 0.95)
  testthat::expect_equal(calculated_reconnection_times, expected_reconnection_times, tolerance = 1e-4)
})