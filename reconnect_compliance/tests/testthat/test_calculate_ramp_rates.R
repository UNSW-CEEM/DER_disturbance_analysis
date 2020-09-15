
testthat::context("Testing calculation of ramp rates.")


load_test_df <- function(text){
  df <- read.table(text = gsub(" ", "", text), sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Calculating ramp rates works",{
  
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
  
  
  ramp_rates <- "                 ts, c_id,       ramp_rate
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
  ramp_rates <- ramp_rates[order(ramp_rates$ts),]
  rownames(ramp_rates) <- NULL
  
  calculated_ramp_rates <- calculate_ramp_rates(normalised_power)
  rownames(calculated_ramp_rates) <- NULL
  
  testthat::expect_equal(calculated_ramp_rates, ramp_rates, tolerance = 1e-4)
})