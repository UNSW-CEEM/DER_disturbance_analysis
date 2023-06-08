
testthat::context("Testing creation of reconnect ramp up profile.")


load_test_file <- function(text) {
  df <- read.table(text = gsub(" ", "", text), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("1 minute ramp up profile, 60 sec duration",{
  
  event_time <- as.POSIXct("2018-01-01 00:00:00", tz = "Australia/Brisbane")
  
  expected_profile_first_3 <- "                 ts, norm_power
                               2018-01-01 00:02:00, 0.00000000
                               2018-01-01 00:02:01, 0.01666667
                               2018-01-01 00:02:02, 0.03333333"
  
  expected_profile_last_3 <- "                  ts, norm_power
                               2018-01-01 00:02:58, 0.9666667
                               2018-01-01 00:02:59, 0.9833333
                               2018-01-01 00:03:00, 1.0000000"
  
  expected_profile_first_3 <- load_test_file(expected_profile_first_3)
  expected_profile_first_3 <- dplyr::mutate(expected_profile_first_3, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))
  expected_profile_last_3 <- load_test_file(expected_profile_last_3)
  expected_profile_last_3 <- dplyr::mutate(expected_profile_last_3, ts = as.POSIXct(ts, tz = "Australia/Brisbane"))

  output_profile <- create_reconnection_profile(event_time, ramp_length_minutes = 1, time_step_seconds = 60)
  
  output_profile_first_3 <- output_profile[1:3, ]
  rownames(output_profile_first_3) <- NULL
  output_profile_last_3 <- output_profile[59:61, ]
  rownames(output_profile_last_3) <- NULL

  testthat::expect_equal(output_profile_first_3, expected_profile_first_3, tolerance = 1e-4)
  testthat::expect_equal(output_profile_last_3, expected_profile_last_3, tolerance = 1e-4)
})