# Do not run this file directly, run testthat.R

testthat::context("Test anti-islanding functions.")

DEFAULT_EVENT_TIMESTAMP <- fastPOSIXct("2022-01-01 12:00:00", tz="Australia/Brisbane")
PRE_EVENT_INTERVAL <- fastPOSIXct("2022-01-01 11:59:55", tz="Australia/Brisbane")
WINDOW_LENGTH <- as.integer(5)
COMBINED_DATA_TEMPLATE <- data.frame(
  c_id=sample.int(1e6, 1),
  v=240,
  vmin=240,
  vmean=240,
  vmax=240,
  ts=DEFAULT_EVENT_TIMESTAMP
)
THRESHOLD_EXCURSION_RESULT_TEMPLATE <- data.frame(
  in_event_window=TRUE,
  vmin_na=FALSE,
  vmax_na=FALSE,
  vmean_na=FALSE,
  antiislanding_v_excursion_2015=NA,
  antiislanding_v_excursion_2020=NA
)

test_that("Test 2015 undervoltage detected", {
  combined_data <- COMBINED_DATA_TEMPLATE
  combined_data['vmin'] <- 150
  expected_result <- append(combined_data, THRESHOLD_EXCURSION_RESULT_TEMPLATE)
  expected_result['antiislanding_v_excursion_2015'] <- 'undervoltage'
  expected_result['antiislanding_v_excursion_2020'] <- 'undervoltage_1'

  result <- detect_voltage_threshold_excursions(combined_data, PRE_EVENT_INTERVAL, WINDOW_LENGTH)
  testthat::expect_equivalent(result, expected_result)
})
test_that("Test 2015 overvoltage 1 detected", {
  combined_data <- COMBINED_DATA_TEMPLATE
  combined_data['vmax'] <- 263
  expected_result <- append(combined_data, THRESHOLD_EXCURSION_RESULT_TEMPLATE)
  expected_result['antiislanding_v_excursion_2015'] <- 'overvoltage_1'

  result <- detect_voltage_threshold_excursions(combined_data, PRE_EVENT_INTERVAL, WINDOW_LENGTH)
  testthat::expect_equivalent(result, expected_result)
})
test_that("Test 2015 overvoltage 2 detected", {
  combined_data <- COMBINED_DATA_TEMPLATE
  combined_data['vmax'] <- 270
  expected_result <- append(combined_data, THRESHOLD_EXCURSION_RESULT_TEMPLATE)
  expected_result['antiislanding_v_excursion_2015'] <- 'overvoltage_2'
  expected_result['antiislanding_v_excursion_2020'] <- 'overvoltage_1'

  result <- detect_voltage_threshold_excursions(combined_data, PRE_EVENT_INTERVAL, WINDOW_LENGTH)
  testthat::expect_equivalent(result, expected_result)
})

test_that("Test 2020 undervoltage 2 detected", {
  combined_data <- COMBINED_DATA_TEMPLATE
  combined_data['vmin'] <- 50
  expected_result <- append(combined_data, THRESHOLD_EXCURSION_RESULT_TEMPLATE)
  expected_result['antiislanding_v_excursion_2015'] <- 'undervoltage'
  expected_result['antiislanding_v_excursion_2020'] <- 'undervoltage_2'

  result <- detect_voltage_threshold_excursions(combined_data, PRE_EVENT_INTERVAL, WINDOW_LENGTH)
  testthat::expect_equivalent(result, expected_result)
})
test_that("Test 2020 undervoltage 1 detected", {
  combined_data <- COMBINED_DATA_TEMPLATE
  combined_data['vmin'] <- 150
  expected_result <- append(combined_data, THRESHOLD_EXCURSION_RESULT_TEMPLATE)
  expected_result['antiislanding_v_excursion_2015'] <- 'undervoltage'
  expected_result['antiislanding_v_excursion_2020'] <- 'undervoltage_1'

  result <- detect_voltage_threshold_excursions(combined_data, PRE_EVENT_INTERVAL, WINDOW_LENGTH)
  testthat::expect_equivalent(result, expected_result)
})
test_that("Test 2020 overvoltage 1 detected", {
  combined_data <- COMBINED_DATA_TEMPLATE
  combined_data['vmax'] <- 270
  expected_result <- append(combined_data, THRESHOLD_EXCURSION_RESULT_TEMPLATE)
  expected_result['antiislanding_v_excursion_2015'] <- 'overvoltage_2'
  expected_result['antiislanding_v_excursion_2020'] <- 'overvoltage_1'

  result <- detect_voltage_threshold_excursions(combined_data, PRE_EVENT_INTERVAL, WINDOW_LENGTH)
  testthat::expect_equivalent(result, expected_result)
})
test_that("Test 2020 overvoltage 2 detected", {
  combined_data <- COMBINED_DATA_TEMPLATE
  combined_data['vmax'] <- 280
  expected_result <- append(combined_data, THRESHOLD_EXCURSION_RESULT_TEMPLATE)
  expected_result['antiislanding_v_excursion_2015'] <- 'overvoltage_2'
  expected_result['antiislanding_v_excursion_2020'] <- 'overvoltage_2'

  result <- detect_voltage_threshold_excursions(combined_data, PRE_EVENT_INTERVAL, WINDOW_LENGTH)
  testthat::expect_equivalent(result, expected_result)
})
