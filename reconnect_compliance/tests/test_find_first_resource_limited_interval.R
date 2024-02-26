testthat::context("Testing calculation of ramp rates.")


load_test_df <- function(text) {
  df <- read.table(text = gsub(" ", "", text), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Calculating ramp rates works", {
  ramp_rates <- "                  ts, c_id,      ramp_rate
                  2018-01-01 00:01:00,    1,           NA
                  2018-01-01 00:02:00,    1,           0.04
                  2018-01-01 00:03:00,    1,           0.95
                  2018-01-01 00:04:00,    1,           0.01
                  2018-01-01 00:01:00,    2,           NA
                  2018-01-01 00:02:00,    2,           0.04
                  2018-01-01 00:03:00,    2,           0.56
                  2018-01-01 00:04:00,    2,           0.37
                  2018-01-01 00:01:00,    3,           NA
                  2018-01-01 00:02:00,    3,           -0.98
                  2018-01-01 00:03:00,    3,           0.02
                  2018-01-01 00:04:00,    3,           0.67
                  2018-01-01 00:05:00,    3,           0.05
                  2018-01-01 00:05:00,    4,           NA"

  ramp_rates <- load_test_df(ramp_rates)
  ramp_rates <- dplyr::mutate(ramp_rates, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))

  first_resource_limited_interval <- " c_id, resource_limited_interval
                                          1,       2018-01-01 00:04:00
                                          3,       2018-01-01 00:05:00"

  first_resource_limited_interval <- load_test_df(first_resource_limited_interval)
  first_resource_limited_interval <- first_resource_limited_interval[order(first_resource_limited_interval$c_id),]
  rownames(first_resource_limited_interval) <- NULL
  first_resource_limited_interval <- dplyr::mutate(
    first_resource_limited_interval,
    resource_limited_interval = as.POSIXct(resource_limited_interval, tz = "Australia/Brisbane")
  )

  calculated_resource_limited_interval <- find_first_resource_limited_interval(
    ramp_rates,
    ramp_rate_change_threshold = -.25
  )
  rownames(calculated_resource_limited_interval) <- NULL

  testthat::expect_equal(calculated_resource_limited_interval, first_resource_limited_interval, tolerance = 1e-4)
})
