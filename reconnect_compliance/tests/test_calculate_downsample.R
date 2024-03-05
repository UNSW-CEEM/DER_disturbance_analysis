testthat::context("Testing downsampling from 1s to 5s for reconnection compliance calculations.")

load_test_df <- function(text) {
  df <- read.table(text = gsub(" ",  "", text), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Downsampling 1s to 5s works", {
  post_event_response <- "ts,                 c_id,  c_id_daily_norm_power,pre_event_norm_power,d
                          2018-01-01 00:01:00,   1,   NA,                   0.1,   1
                          2018-01-01 00:01:01,   1,   0.0020,               0.1,   1
                          2018-01-01 00:01:02,   1,   0.0025,               0.1,   1
                          2018-01-01 00:01:03,   1,   0.0020,               0.1,   1
                          2018-01-01 00:01:04,   1,   0.0021,               0.1,   1
                          2018-01-01 00:01:05,   1,   0.0023,               0.1,   1
                          2018-01-01 00:01:06,   1,   0.0024,               0.1,   1
                          2018-01-01 00:01:07,   1,   0.0023,               0.1,   1
                          2018-01-01 00:01:08,   1,   0.0022,               0.1,   1
                          2018-01-01 00:01:09,   1,   0.0020,               0.1,   1
                          2018-01-01 00:01:10,   1,   0.0023,               0.1,   1
                          2018-01-01 00:01:11,   1,   0.0024,               0.1,   1
                          2018-01-01 00:01:12,   1,   0.0024,               0.1,   1
                          2018-01-01 00:01:13,   1,   0.0022,               0.1,   1
                          2018-01-01 00:01:14,   1,   0.0025,               0.1,   1
                          2018-01-01 00:01:15,   1,   0.0026,               0.1,   1
                          2018-01-01 00:01:16,   1,   0.0030,               0.1,   1
                          2018-01-01 00:01:17,   1,   0.0032,               0.1,   1
                          2018-01-01 00:01:19,   1,   0.0030,               0.1,   1"
  
  # first time interval (2018-01-01 00:01:00) to test downsampling with NAs
  # last time interval (2018-01-01 00:01:15) to test downsampling with a missing time step
  
  post_event_response <- load_test_df(post_event_response)
  post_event_response <- dplyr::mutate(post_event_response, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))
  
  post_event_response_new <- "ts,                  c_id,  c_id_daily_norm_power,pre_event_norm_power
                              2018-01-01 00:01:00,   1,   0.00215,                  0.1
                              2018-01-01 00:01:05,   1,   0.00224,                  0.1
                              2018-01-01 00:01:10,   1,   0.00236,                  0.1
                              2018-01-01 00:01:15,   1,   0.00295,                  0.1"
 
  post_event_response_new <- load_test_df(post_event_response_new)
  post_event_response_new <- dplyr::mutate(post_event_response_new, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))
  post_event_response_new <- post_event_response_new[order(post_event_response_new$ts),]
  rownames(post_event_response_new) <- NULL
  
  calculated_downsampled_post_event_response <- downsample_for_reconnection(post_event_response)
  rownames(calculated_downsampled_post_event_response) <- NULL
  
  print("Calculated Downsampled Post Event Response:")
  print(calculated_downsampled_post_event_response)
  
  print("Expected Post Event Response (after downsampling):")
  print(post_event_response_new)
  
  testthat::expect_equal(calculated_downsampled_post_event_response, post_event_response_new, tolerance = 1e-4)
})
