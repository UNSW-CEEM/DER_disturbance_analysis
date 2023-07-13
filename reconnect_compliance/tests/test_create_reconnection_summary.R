
testthat::context("Testing creating the reconnection summary.")


load_test_df <- function(text) {
  text <- gsub(" ", "", text)
  text <- gsub("~", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Categorising a circuit with 5s data, and just a breif ramp rate violation works.",{

  event_time <- as.POSIXct("2018-01-01 00:01:00", tz = "Australia/Brisbane")

  ramp_rates <- "                       ts, c_id, d, response_category, c_id_daily_norm_power, pre_event_norm_power
                       2018-01-01~00:01:00,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:05,    1, 5,    3~Drop~to~Zero,                  0.01,                 0.40
                       2018-01-01~00:01:10,    1, 5,    3~Drop~to~Zero,                  0.28,                 0.40
                       2018-01-01~00:01:15,    1, 5,    3~Drop~to~Zero,                  0.30,                 0.40
                       2018-01-01~00:01:20,    1, 5,    3~Drop~to~Zero,                  0.32,                 0.40
                       2018-01-01~00:01:25,    1, 5,    3~Drop~to~Zero,                  0.34,                 0.40
                       2018-01-01~00:01:30,    1, 5,    3~Drop~to~Zero,                  0.36,                 0.40
                       2018-01-01~00:01:35,    1, 5,    3~Drop~to~Zero,                  0.38,                 0.40
                       2018-01-01~00:01:40,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:45,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40"


  ramp_rates <- load_test_df(ramp_rates)
  ramp_rates <- dplyr::mutate(ramp_rates, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))


  expected_results <- "c_id,  reconnection_compliance_status, reconnection_time, ramp_above_threshold, pre_event_daily_norm_power
                          1,                          Unsure,         0.5833333,                 0.27,                        0.4"

  expected_results <- load_test_df(expected_results)

  calculated_results <- create_reconnection_summary(ramp_rates, event_time,
                                                    disconnecting_threshold = 0.05,
                                                    reconnect_threshold = 0.95,
                                                    ramp_rate_threshold = 0.5,
                                                    ramp_threshold_for_compliance = 0.2,
                                                    ramp_threshold_for_non_compliance = 0.3,
                                                    ramp_rate_change_resource_limit_threshold = -0.1)


  testthat::expect_equivalent(calculated_results, expected_results, tolerance = 1e-4)
})


testthat::test_that("Categorising a circuit with 5s data, sustained ramp rate violation.",{

  event_time <- as.POSIXct("2018-01-01 00:01:00", tz = "Australia/Brisbane")

  ramp_rates <- "                       ts, c_id, d, response_category, c_id_daily_norm_power, pre_event_norm_power
                       2018-01-01~00:01:00,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:05,    1, 5,    3~Drop~to~Zero,                  0.01,                 0.40
                       2018-01-01~00:01:10,    1, 5,    3~Drop~to~Zero,                  0.28,                 0.40
                       2018-01-01~00:01:15,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:20,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:25,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:30,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:35,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:40,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40
                       2018-01-01~00:01:45,    1, 5,    3~Drop~to~Zero,                  0.40,                 0.40"


  ramp_rates <- load_test_df(ramp_rates)
  ramp_rates <- dplyr::mutate(ramp_rates, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))


  expected_results <- "c_id,  reconnection_compliance_status, reconnection_time, ramp_above_threshold, pre_event_daily_norm_power
                          1,                   Non~Compliant,         0.1666667,                 0.39,                        0.4"

  expected_results <- load_test_df(expected_results)

  calculated_results <- create_reconnection_summary(ramp_rates, event_time,
                                                    disconnecting_threshold = 0.05,
                                                    reconnect_threshold = 0.95,
                                                    ramp_rate_threshold = 0.5,
                                                    ramp_threshold_for_compliance = 0.2,
                                                    ramp_threshold_for_non_compliance = 0.3,
                                                    ramp_rate_change_resource_limit_threshold = -0.1)

  testthat::expect_equivalent(calculated_results, expected_results, tolerance = 1e-4)
})


testthat::test_that("Categorising a circuit with 60s data, no ramp rate violation works.",{

  event_time <- as.POSIXct("2018-01-01 00:01:00", tz = "Australia/Brisbane")

  ramp_rates <- "                  ts, c_id,  d, response_category, c_id_daily_norm_power, pre_event_norm_power
                  2018-01-01~00:01:00,    1, 60,    3~Drop~to~Zero,                  0.90,                 0.90
                  2018-01-01~00:02:00,    1, 60,    3~Drop~to~Zero,                  0.01,                 0.90
                  2018-01-01~00:03:00,    1, 60,    3~Drop~to~Zero,                  0.28,                 0.90
                  2018-01-01~00:04:00,    1, 60,    3~Drop~to~Zero,                  0.30,                 0.90
                  2018-01-01~00:05:00,    1, 60,    3~Drop~to~Zero,                  0.32,                 0.90
                  2018-01-01~00:06:00,    1, 60,    3~Drop~to~Zero,                  0.34,                 0.90
                  2018-01-01~00:07:00,    1, 60,    3~Drop~to~Zero,                  0.36,                 0.90
                  2018-01-01~00:08:00,    1, 60,    3~Drop~to~Zero,                  0.38,                 0.90
                  2018-01-01~00:09:00,    1, 60,    3~Drop~to~Zero,                  0.90,                 0.90
                  2018-01-01~00:10:05,    1, 60,    3~Drop~to~Zero,                  0.90,                 0.90"


  ramp_rates <- load_test_df(ramp_rates)
  ramp_rates <- dplyr::mutate(ramp_rates, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))


  expected_results <- "c_id,  reconnection_compliance_status, reconnection_time, ramp_above_threshold, pre_event_daily_norm_power
                          1,                       Compliant,               7.0,                  0.0,                        0.9"

  expected_results <- load_test_df(expected_results)

  calculated_results <- create_reconnection_summary(ramp_rates, event_time,
                                                    disconnecting_threshold = 0.05,
                                                    reconnect_threshold = 0.95,
                                                    ramp_rate_threshold = 0.5,
                                                    ramp_threshold_for_compliance = 0.2,
                                                    ramp_threshold_for_non_compliance = 0.3,
                                                    ramp_rate_change_resource_limit_threshold = -0.1)

  testthat::expect_equivalent(calculated_results, expected_results, tolerance = 1e-4)
})


testthat::test_that("Categorising a circuit with 5s data, sustained ramp rate violation.",{

  event_time <- as.POSIXct("2018-01-01 00:01:00", tz = "Australia/Brisbane")

  ramp_rates <- "                  ts, c_id,  d, response_category, c_id_daily_norm_power, pre_event_norm_power
                  2018-01-01~00:01:00,    1, 60,    3~Drop~to~Zero,                  0.90,                 0.90
                  2018-01-01~00:02:00,    1, 60,    3~Drop~to~Zero,                  0.01,                 0.90
                  2018-01-01~00:03:00,    1, 60,    3~Drop~to~Zero,                  0.60,                 0.90
                  2018-01-01~00:04:00,    1, 60,    3~Drop~to~Zero,                  0.60,                 0.90
                  2018-01-01~00:05:00,    1, 60,    3~Drop~to~Zero,                  0.60,                 0.90
                  2018-01-01~00:06:00,    1, 60,    3~Drop~to~Zero,                  0.60,                 0.90
                  2018-01-01~00:07:00,    1, 60,    3~Drop~to~Zero,                  0.60,                 0.90
                  2018-01-01~00:08:00,    1, 60,    3~Drop~to~Zero,                  0.60,                 0.90
                  2018-01-01~00:09:00,    1, 60,    3~Drop~to~Zero,                  0.90,                 0.90
                  2018-01-01~00:10:05,    1, 60,    3~Drop~to~Zero,                  0.90,                 0.90"


  ramp_rates <- load_test_df(ramp_rates)
  ramp_rates <- dplyr::mutate(ramp_rates, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))


  expected_results <- "c_id,  reconnection_compliance_status, reconnection_time, ramp_above_threshold, pre_event_daily_norm_power
                          1,                   Non~Compliant,               7.0,                 0.59,                        0.9"

  expected_results <- load_test_df(expected_results)

  calculated_results <- create_reconnection_summary(ramp_rates, event_time,
                                                    disconnecting_threshold = 0.05,
                                                    reconnect_threshold = 0.95,
                                                    ramp_rate_threshold = 0.5,
                                                    ramp_threshold_for_compliance = 0.2,
                                                    ramp_threshold_for_non_compliance = 0.3,
                                                    ramp_rate_change_resource_limit_threshold = -0.1)


  testthat::expect_equivalent(calculated_results, expected_results, tolerance = 1e-4)
})
