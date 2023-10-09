testthat::context("Testing finding first resource limited interval.")


load_test_df <- function(text){
  df <- read.table(text = gsub(" ", "", text), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Finding resource limited interval works.",{
  event_time <- as.POSIXct("2018-01-01 00:01:00", tz = "Australia/Brisbane")

  disconnect_threshold <- 0.05
  reconnect_threshold <- 0.95

  ramp_rates <- "                       ts, c_id,  ramp_rate
                       2018-01-01 00:01:00,    1,         NA
                       2018-01-01 00:02:00,    1,      -0.49
                       2018-01-01 00:03:00,    1,      0.466
                       2018-01-01 00:04:00,    1,      0.514
                       2018-01-01 00:01:00,    2,         NA
                       2018-01-01 00:02:00,    2,       0.04
                       2018-01-01 00:03:00,    2,       0.56
                       2018-01-01 00:04:00,    2,       0.37
                       2018-01-01 00:01:00,    3,         NA
                       2018-01-01 00:02:00,    3,      -0.98
                       2018-01-01 00:03:00,    3,       0.49
                       2018-01-01 00:04:00,    3,       0.10
                       2018-01-01 00:05:00,    3,       0.60
                       2018-01-01 00:05:00,    4,         NA"

  ramp_rates <- load_test_df(ramp_rates)
  ramp_rates <- dplyr::mutate(ramp_rates, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))

  expected_results <- "c_id,  resource_limited_interval
                          2,        2018-01-01 00:04:00
                          3,        2018-01-01 00:04:00"

  expected_results <- load_test_df(expected_results)
  expected_results <- dplyr::mutate(
    expected_results,
    resource_limited_interval = as.POSIXct(resource_limited_interval, tz = "Australia/Brisbane")
  )

  calculated_results <- find_first_resource_limited_interval(ramp_rates, ramp_rate_change_threshold = -0.1)

  testthat::expect_equal(calculated_results, expected_results, tolerance = 1e-4)
})
