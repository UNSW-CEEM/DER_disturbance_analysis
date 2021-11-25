test_that("Test the identify islands function" ,{
  # Test input data
  c_id <- c(101, 2, 3000, 3, 4, 5)
  combined_data <- data.frame(c_id)
  first_timestamp <- c(1611470760257, 1611470760257, 1611470760257, 1611470760257, 1611470760257, 1611470760257)
  GridFaultContactorTrip <- c(1, 1, 1, 1, 1, 1)
  SYNC_a038_DoOpenArguments <- c(1, NA, 1, 0, 1, 0)
  SYNC_a005_vfCheckUnderVoltage <- c(1, NA, 1, NA, 1, NA)
  SYNC_a010_vfCheckFreqWobble <- c(100, 1, 1, 1, NA, NA)
  alert_data <- data.frame(c_id, first_timestamp, GridFaultContactorTrip, SYNC_a038_DoOpenArguments, 
                           SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  event_time <- as.POSIXct("2021-01-24 16:46:00", tz = "Australia/Brisbane")
  
  out <- identify_islanded_sites(combined_data, alert_data, event_time)
  Islanded <- c(1, 1, 1, 1, 1, 1)
  expected_output <- data.frame(c_id, Islanded, SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  expect_equal(out, expected_output, tolerance=0.001)
})

