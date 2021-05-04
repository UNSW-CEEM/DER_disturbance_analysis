
testthat::context("Testing creating the reconnection summary.")


load_test_df <- function(text){
  text <- gsub(" ", "", text)
  text <- gsub("~", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Categorising a circuit with 5s data, and just a breif ramp rate violation works.",{
  
  event_time <- as.POSIXct("2018-01-01 00:01:00", tz = "Australia/Brisbane")
  
  ramp_rates <- "                       ts, c_id,  response_category, c_id_daily_norm_power, pre_event_norm_power
                       2018-01-01~00:01:00,    1,     3~Drop~to~Zero,                  0.40,                 0.40 
                       2018-01-01~00:01:05,    1,     3~Drop~to~Zero,                  0.01,                 0.40
                       2018-01-01~00:01:10,    1,     3~Drop~to~Zero,                  0.28,                 0.40
                       2018-01-01~00:01:15,    1,     3~Drop~to~Zero,                  0.30,                 0.40
                       2018-01-01~00:01:20,    1,     3~Drop~to~Zero,                  0.32,                 0.40
                       2018-01-01~00:01:25,    1,     3~Drop~to~Zero,                  0.34,                 0.40
                       2018-01-01~00:01:30,    1,     3~Drop~to~Zero,                  0.36,                 0.40
                       2018-01-01~00:01:35,    1,     3~Drop~to~Zero,                  0.38,                 0.40
                       2018-01-01~00:01:40,    1,     3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:45,    1,     3~Drop~to~Zero,                  0.40,                 0.40"
  
  
  ramp_rates <- load_test_df(ramp_rates)
  ramp_rates <- dplyr::mutate(ramp_rates, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))
  
  
  expected_results <- "c_id,  reconnection_compliance_status, reconnection_time, max_reconnection_ramp_rate, pre_event_daily_norm_power
                          1,                   Non~Compliant,         0.5833333,                       3.24,                        0.4"
  
  expected_results <- load_test_df(expected_results)
  
  calculated_results <- create_reconnection_summary(ramp_rates, event_time,
                                                    disconnecting_threshold = 0.05,
                                                    reconnect_threshold = 0.95,
                                                    reconnection_time_threshold_for_compliance = 4,
                                                    reconnection_time_threshold_for_non_compliance = 2,
                                                    ramp_rate_threshold_for_compliance = 0.3,
                                                    ramp_rate_threshold_for_non_compliance = 0.5,
                                                    ramp_rate_change_resource_limit_threshold = -0.1)
  browser()
  
  testthat::expect_equal(calculated_results, expected_results, tolerance = 1e-4)
})