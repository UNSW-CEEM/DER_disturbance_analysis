testthat::context("Testing calculation of reconnection max ramp rate.")


load_test_df <- function(text) {
  df <- read.table(text = gsub(" ", "", text), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Calculating reconnection max ramp rate works", {
  event_time <- as.POSIXct("2018-01-01 00:01:00", tz = "Australia/Brisbane")

  disconnect_threshold <- 0.05
  reconnect_threshold <- 0.95

  normalised_power <- "                 ts, c_id,  d, c_id_daily_norm_power, pre_event_norm_power, resource_limited_interval, ramp_rate
                       2018-01-01 00:01:00,    1, 60,                  0.50,                 0.50,                        NA,        NA
                       2018-01-01 00:02:00,    1, 60,                  0.01,                 0.50,                        NA,     -0.49
                       2018-01-01 00:03:00,    1, 60,                  0.476,                0.50,                        NA,     0.466
                       2018-01-01 00:04:00,    1, 60,                  0.99,                 0.50,                        NA,     0.514
                       2018-01-01 00:01:00,    2, 60,                  1.00,                 1.00,                        NA,        NA
                       2018-01-01 00:02:00,    2, 60,                  0.04,                 1.00,                        NA,      0.04
                       2018-01-01 00:03:00,    2, 60,                  0.60,                 1.00,                        NA,      0.56
                       2018-01-01 00:04:00,    2, 60,                  0.97,                 1.00,                        NA,      0.37
                       2018-01-01 00:01:00,    3, 20,                  1.00,                 1.00,       2018-01-01 00:02:20,        NA
                       2018-01-01 00:01:20,    3, 20,                  0.01,                 1.00,       2018-01-01 00:02:20,     -0.98
                       2018-01-01 00:01:40,    3, 20,                  0.21,                 1.00,       2018-01-01 00:02:20,      0.60
                       2018-01-01 00:02:00,    3, 20,                  0.41,                 1.00,       2018-01-01 00:02:20,      0.60
                       2018-01-01 00:02:20,    3, 20,                  0.30,                 1.00,       2018-01-01 00:02:20,     -0.33
                       2018-01-01 00:02:40,    3, 20,                  1.00,                 1.00,       2018-01-01 00:02:20,       2.1
                       2018-01-01 00:05:00,    4, 60,                  0.50,                 1.00,                        NA,        NA"

  normalised_power <- load_test_df(normalised_power)
  normalised_power <- dplyr::mutate(normalised_power, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))
  normalised_power <- dplyr::mutate(
    normalised_power,
    resource_limited_interval = as.POSIXct(resource_limited_interval, tz = "Australia/Brisbane")
  )

  expected_reconnection_times <- "c_id, ramp_above_threshold
                                     1,                  0.0
                                     2,                 0.56
                                     3,                 0.40"

  expected_reconnection_times <- load_test_df(expected_reconnection_times)

  calculated_reconnection_times <- calculate_total_ramp_while_exceeding_ramp_rate_compliance_threshold(
    normalised_power,
    event_time,
    disconnect_threshold = 0.05,
    reconnect_threshold = 0.95,
    ramp_rate_threshold = 0.50
  )

  testthat::expect_equal(calculated_reconnection_times, expected_reconnection_times, tolerance = 1e-4)
})
