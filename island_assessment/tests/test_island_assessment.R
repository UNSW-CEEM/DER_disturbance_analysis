test_that("Test identify_islanded_sites for a site with GridFaultContactorTrip=1" ,{
  # Test input data
  c_id <- c(101)
  combined_data <- data.frame(c_id)
  first_timestamp <- c(1611470760257)
  GridFaultContactorTrip <- c(1)
  SYNC_a038_DoOpenArguments <- c(0)
  SYNC_a005_vfCheckUnderVoltage <- c(1)
  SYNC_a010_vfCheckFreqWobble <- c(100)
  alert_data <- data.frame(c_id, first_timestamp, GridFaultContactorTrip, SYNC_a038_DoOpenArguments, 
                           SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  event_time <- as.POSIXct("2021-01-24 16:46:00", tz = "Australia/Brisbane")
  
  out <- identify_islanded_sites(combined_data, alert_data, event_time)
  Islanded <- c(1)
  expected_output <- data.frame(c_id, Islanded, SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test identify_islanded_sites for a site with SYNC_a038_DoOpenArguments=1 and 
          GridFaultContactorTrip=NA" ,{
  # Test input data
  c_id <- c(2)
  combined_data <- data.frame(c_id)
  first_timestamp <- c(1611470760257)
  GridFaultContactorTrip <- c(NA)
  SYNC_a038_DoOpenArguments <- c(1)
  SYNC_a005_vfCheckUnderVoltage <- c(1)
  SYNC_a010_vfCheckFreqWobble <- c(100)
  alert_data <- data.frame(c_id, first_timestamp, GridFaultContactorTrip, SYNC_a038_DoOpenArguments, 
                           SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  event_time <- as.POSIXct("2021-01-24 16:46:00", tz = "Australia/Brisbane")
  
  out <- identify_islanded_sites(combined_data, alert_data, event_time)
  Islanded <- c(1)
  expected_output <- data.frame(c_id, Islanded, SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test identify_islanded_sites for a site that islanded more than 60s after the event" ,{
  # Test input data
  c_id <- c(3000)
  combined_data <- data.frame(c_id)
  first_timestamp <- c(1611470850257)
  GridFaultContactorTrip <- c(1)
  SYNC_a038_DoOpenArguments <- c(0)
  SYNC_a005_vfCheckUnderVoltage <- c(1)
  SYNC_a010_vfCheckFreqWobble <- c(100)
  alert_data <- data.frame(c_id, first_timestamp, GridFaultContactorTrip, SYNC_a038_DoOpenArguments, 
                           SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  event_time <- as.POSIXct("2021-01-24 16:46:00", tz = "Australia/Brisbane")
  
  out <- identify_islanded_sites(combined_data, alert_data, event_time)
  Islanded <- c(0)
  expected_output <- data.frame(c_id, Islanded, SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test identify_islanded_sites for a site that islanded but didn't have a timestamp" ,{
  # Test input data
  c_id <- c(3000)
  combined_data <- data.frame(c_id)
  first_timestamp <- c(NA)
  GridFaultContactorTrip <- c(1)
  SYNC_a038_DoOpenArguments <- c(0)
  SYNC_a005_vfCheckUnderVoltage <- c(1)
  SYNC_a010_vfCheckFreqWobble <- c(100)
  alert_data <- data.frame(c_id, first_timestamp, GridFaultContactorTrip, SYNC_a038_DoOpenArguments, 
                           SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  event_time <- as.POSIXct("2021-01-24 16:46:00", tz = "Australia/Brisbane")
  
  out <- identify_islanded_sites(combined_data, alert_data, event_time)
  Islanded <- c(1)
  expected_output <- data.frame(c_id, Islanded, SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test identify_islanded_sites for a site with no SYNC_a005_vfCheckUnderVoltage value" ,{
  # Test input data
  c_id <- c(3000)
  combined_data <- data.frame(c_id)
  first_timestamp <- c(1611470760257)
  GridFaultContactorTrip <- c(1)
  SYNC_a038_DoOpenArguments <- c(0)
  SYNC_a005_vfCheckUnderVoltage <- c(NA)
  SYNC_a010_vfCheckFreqWobble <- c(100)
  alert_data <- data.frame(c_id, first_timestamp, GridFaultContactorTrip, SYNC_a038_DoOpenArguments, 
                           SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  event_time <- as.POSIXct("2021-01-24 16:46:00", tz = "Australia/Brisbane")
  
  out <- identify_islanded_sites(combined_data, alert_data, event_time)
  Islanded <- c(1)
  SYNC_a005_vfCheckUnderVoltage <- c(0)
  expected_output <- data.frame(c_id, Islanded, SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test assess_islands with response_category = 'NED' and v, f are normal" ,{
  # Test input data
  c_id <- c(3000)
  ts <- c('2021-05-25 13:05')
  clean <- c(1)
  Islanded <- c(1)
  response_category <- c('NED')
  f <- c(50)
  v <- c(240)
  combined_data <- data.frame(c_id, ts, clean, Islanded, response_category, f, v, stringsAsFactors = FALSE)
  
  out <- as.data.frame(assess_islands(combined_data))
  island_assessment <- c("Otherwise normal")
  expected_output <- data.frame(c_id, clean, island_assessment, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test assess_islands with max_freq = 55Hz" ,{
  # Test input data
  c_id <- c(3000)
  ts <- c('2021-05-25 13:05')
  clean <- c(1)
  Islanded <- c(1)
  response_category <- c('3 Drop to Zero')
  f <- c(55)
  v <- c(240)
  combined_data <- data.frame(c_id, ts, clean, Islanded, response_category, f, v, stringsAsFactors = FALSE)

  out <- as.data.frame(assess_islands(combined_data))
  island_assessment <- c('Gateway curtailed')
  expected_output <- data.frame(c_id, clean, island_assessment, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test assess_islands with min_freq = 49Hz" ,{
  # Test input data
  c_id <- c(3000)
  ts <- c('2021-05-25 13:05')
  clean <- c(1)
  Islanded <- c(1)
  response_category <- c('3 Drop to Zero')
  f <- c(49)
  v <- c(240)
  combined_data <- data.frame(c_id, ts, clean, Islanded, response_category, f, v, stringsAsFactors = FALSE)
  
  out <- as.data.frame(assess_islands(combined_data))
  island_assessment <- c('Frequency disruption')
  expected_output <- data.frame(c_id, clean, island_assessment, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test assess_islands with fmin = 48Hz but minimum f = 50 Hz" ,{
  # Test input data
  c_id <- c(3000)
  ts <- c('2021-05-25 13:05')
  clean <- c(1)
  Islanded <- c(1)
  response_category <- c('3 Drop to Zero')
  f <- c(50)
  fmin <- c(48)
  v <- c(240)
  combined_data <- data.frame(c_id, ts, clean, Islanded, response_category, f, fmin, v, stringsAsFactors = FALSE)
  
  out <- as.data.frame(assess_islands(combined_data))
  island_assessment <- c('Frequency disruption')
  expected_output <- data.frame(c_id, clean, island_assessment, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test assess_islands with max_voltage = 270Hz" ,{
  # Test input data
  c_id <- c(30)
  ts <- c('2021-05-25 13:05')
  clean <- c(1)
  Islanded <- c(1)
  response_category <- c('3 Drop to Zero')
  f <- c(50)
  v <- c(270)
  combined_data <- data.frame(c_id, ts, clean, Islanded, response_category, f, v, stringsAsFactors = FALSE)
  
  out <- as.data.frame(assess_islands(combined_data))
  island_assessment <- c('Voltage disruption')
  expected_output <- data.frame(c_id, clean, island_assessment, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test assess_islands for site with no other disruptions" ,{
  # Test input data
  c_id <- c(30)
  ts <- c('2021-05-25 13:05')
  clean <- c(1)
  Islanded <- c(1)
  response_category <- c('4 Disconnect')
  f <- c(50)
  v <- c(241)
  combined_data <- data.frame(c_id, ts, clean, Islanded, response_category, f, v, stringsAsFactors = FALSE)
  
  out <- as.data.frame(assess_islands(combined_data))
  island_assessment <- c('PV disconnect')
  expected_output <- data.frame(c_id, clean, island_assessment, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test replace_response_with_alert with neither alert flag" ,{
  # Test input data
  c_id <- c(30)
  Islanded <- c(1)
  response_category <- c('4 Disconnect')
  SYNC_a005_vfCheckUnderVoltage <- c(0)
  SYNC_a010_vfCheckFreqWobble <- c(0)

  combined_data <- data.frame(c_id, Islanded, response_category, SYNC_a005_vfCheckUnderVoltage, 
                              SYNC_a010_vfCheckFreqWobble, stringsAsFactors = FALSE)
  
  out <- replace_response_with_alert(combined_data)
  islanding_alert <- c('Islanded - Other')
  expected_output <- data.frame(c_id, Islanded, response_category, islanding_alert, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test replace_response_with_alert for under voltage alert" ,{
  # Test input data
  c_id <- c(30)
  Islanded <- c(1)
  response_category <- c('4 Disconnect')
  SYNC_a005_vfCheckUnderVoltage <- c(1)
  SYNC_a010_vfCheckFreqWobble <- c(0)
  
  combined_data <- data.frame(c_id, Islanded, response_category, SYNC_a005_vfCheckUnderVoltage, 
                              SYNC_a010_vfCheckFreqWobble, stringsAsFactors = FALSE)
  
  out <- replace_response_with_alert(combined_data)
  islanding_alert <- c('Islanded - Under Volt alert')
  expected_output <- data.frame(c_id, Islanded, response_category, islanding_alert, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test replace_response_with_alert for freq wobble alert" ,{
  # Test input data
  c_id <- c(30)
  Islanded <- c(1)
  response_category <- c('4 Disconnect')
  SYNC_a005_vfCheckUnderVoltage <- c(0)
  SYNC_a010_vfCheckFreqWobble <- c(100)
  
  combined_data <- data.frame(c_id, Islanded, response_category, SYNC_a005_vfCheckUnderVoltage, 
                              SYNC_a010_vfCheckFreqWobble, stringsAsFactors = FALSE)
  
  out <- replace_response_with_alert(combined_data)
  islanding_alert <- c('Islanded - Freq Wobble')
  expected_output <- data.frame(c_id, Islanded, response_category, islanding_alert, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test high level classify_islands fn" ,{
  # Test input data

  c_id <- c(3000)
  ts <- as.POSIXct("2021-01-24 16:47:00", tz = "Australia/Brisbane")
  d <- 60
  clean <- c(1)
  response_category <- c('3 Drop to Zero')
  f <- c(52)
  v <- c(240)
  combined_data <- data.frame(c_id, ts, d, clean, response_category, f, v, stringsAsFactors = FALSE)
  
  first_timestamp <- c(1611470760257)
  GridFaultContactorTrip <- c(1)
  SYNC_a038_DoOpenArguments <- c(0)
  SYNC_a005_vfCheckUnderVoltage <- c(1)
  SYNC_a010_vfCheckFreqWobble <- c(100)
  alert_data <- data.frame(c_id, first_timestamp, GridFaultContactorTrip, SYNC_a038_DoOpenArguments, 
                           SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  event_time <- as.POSIXct("2021-01-24 16:46:00", tz = "Australia/Brisbane")
  window_length <- 5
  
  out <- classify_islands(combined_data, alert_data, event_time, window_length)
  Islanded <- c(1)
  island_assessment <- c('Frequency disruption')
  islanding_alert <- c('Islanded - Freq Wobble')
  expected_output <- data.frame(c_id, ts, d, clean, response_category, f, v, Islanded,
                                island_assessment, islanding_alert, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})

test_that("Test high level classify_islands fn for a non-islanded site" ,{
  # Test input data
  
  c_id <- c(3000)
  ts <- as.POSIXct("2021-01-24 16:47:00", tz = "Australia/Brisbane")
  d <- 60
  clean <- c(1)
  response_category <- c('3 Drop to Zero')
  f <- c(52)
  v <- c(240)
  combined_data <- data.frame(c_id, ts, d, clean, response_category, f, v, stringsAsFactors = FALSE)
  
  first_timestamp <- c(1611470760257)
  GridFaultContactorTrip <- c(0)
  SYNC_a038_DoOpenArguments <- c(0)
  SYNC_a005_vfCheckUnderVoltage <- c(NA)
  SYNC_a010_vfCheckFreqWobble <- c(NA)
  alert_data <- data.frame(c_id, first_timestamp, GridFaultContactorTrip, SYNC_a038_DoOpenArguments, 
                           SYNC_a005_vfCheckUnderVoltage, SYNC_a010_vfCheckFreqWobble)
  event_time <- as.POSIXct("2021-01-24 16:46:00", tz = "Australia/Brisbane")
  window_length <- 5
  
  out <- classify_islands(combined_data, alert_data, event_time, window_length)
  Islanded <- c(0)
  island_assessment <- c(NA_character_)
  islanding_alert <- c("NA")
  expected_output <- data.frame(c_id, ts, d, clean, response_category, f, v, Islanded,
                                island_assessment, islanding_alert, stringsAsFactors = FALSE)
  expect_equal(out, expected_output, tolerance=0.001)
})