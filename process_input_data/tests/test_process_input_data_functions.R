context("Testing the DER event analysis underlying data manipulation functions")

test_that("Test the preprocessing of the timeseries data", {
  # Test input data
  c_id <- c("101", "101")
  ts <- c("2018-01-01 00:01:00", "2018-01-01 00:02:00")
  e <- c("1015.0", "-1010.0")
  v <- c("240.1", "240.2")
  f <- c("50.1", "50.0")
  d <- c(60, 60)
  test_time_series_data <- data.frame(c_id, ts, e, d, v, f, stringsAsFactors = FALSE)
  # Test output data
  c_id <- c("101", "101")
  ts <- c(
    as.POSIXct(strptime("2018-01-01 10:01:00", "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")),
    as.POSIXct(strptime("2018-01-01 10:02:00", "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"))
  )
  e <- c(1015.0, -1010.0)
  v <- c(240.1, 240.2)
  f <- c(50.1, 50.0)
  d <- c(60, 60)
  expected_answer <- data.frame(c_id, ts, e, d, v, f, stringsAsFactors = FALSE)
  # Call processing function
  processed_time_series <- process_time_series_data(test_time_series_data)
  # Test the answer matches the expected answer
  expect_equal(processed_time_series, expected_answer)
})

test_that("Test assertion of site data assumptions, s_state must be NSW, ACT, SA etc", {
  # Test input data
  site_id <- c(101, 101, 300)
  s_state <- c("NSW", "sA", "SA")
  s_postcode <- c(2031, 2031, 2031)
  dc <- c(60, 60, 10)
  ac <- c(59, 60, 8)
  pv_installation_year_month <- c("2017-01", "2017-10", "2018-01")
  manufacturer <- c("SMA", "SMA", "ABB")
  model <- c("1", "2", "3")
  test_site_details <- data.frame(
    site_id,
    s_state,
    s_postcode,
    dc,
    ac,
    pv_installation_year_month,
    manufacturer,
    model,
    stringsAsFactors = FALSE
  )
  expect_error(process_raw_site_details(test_site_details))
})

test_that("Test assertion of site data assumptions, each site has only one distinct value for s_postcode", {
  # Test input data
  site_id <- c(101, 101, 300)
  s_state <- c("NSW", "NSW", "SA")
  s_postcode <- c(2031, 2032, 2031)
  dc <- c(60, 60, 10)
  ac <- c(59, 60, 8)
  pv_installation_year_month <- c("2017-01", "2017-10", "2018-01")
  manufacturer <- c("SMA", "SMA", "ABB")
  model <- c("1", "2", "3")
  test_site_details <- data.frame(
    site_id,
    s_state,
    s_postcode,
    dc,
    ac,
    pv_installation_year_month,
    manufacturer,
    model,
    stringsAsFactors = FALSE
  )
  expect_error(process_raw_site_details(test_site_details))
})

test_that("Test assertion of site data assumptions, each site only has one distinct value for s_state", {
  # Test input data
  site_id <- c(101, 101, 300)
  s_state <- c("NSW", "SA", "SA")
  s_postcode <- c(2031, 2031, 2031)
  dc <- c(60, 60, 10)
  ac <- c(59, 60, 8)
  pv_installation_year_month <- c("2017-01", "2017-10", "2018-01")
  manufacturer <- c("SMA", "SMA", "ABB")
  model <- c("1", "2", "3")
  test_site_details <- data.frame(
    site_id,
    s_state,
    s_postcode,
    dc,
    ac,
    pv_installation_year_month,
    manufacturer,
    model,
    stringsAsFactors = FALSE
  )
  expect_error(process_raw_site_details(test_site_details))
})

test_that("Test assertion of site data assumptions, dc can be converted to type numeric without generating NAs", {
  # Test input data
  site_id <- c(101, 101, 300)
  s_state <- c("NSW", "SA", "SA")
  s_postcode <- c(2031, 2031, 2031)
  dc <- c(60, '', 10)
  ac <- c(59, 60, 8)
  pv_installation_year_month <- c("2017-01", "2017-10", "2018-01")
  manufacturer <- c("SMA", "SMA", "ABB")
  model <- c("1", "2", "3")
  test_site_details <- data.frame(
    site_id,
    s_state,
    s_postcode,
    dc,
    ac,
    pv_installation_year_month,
    manufacturer,
    model,
    stringsAsFactors = FALSE
  )
  expect_error(process_raw_site_details(test_site_details))
})

test_that("Test assertion of site data assumptions, ac can be converted to type numeric without generating NAs", {
  # Test input data
  site_id <- c(101, 101, 300)
  s_state <- c("NSW", "SA", "SA")
  s_postcode <- c(2031, 2031, 2031)
  dc <- c(60, 60, 10)
  ac <- c('x', 60, 8)
  pv_installation_year_month <- c("2017-01", "2017-10", "2018-01")
  manufacturer <- c("SMA", "SMA", "ABB")
  model <- c("1", "2", "3")
  test_site_details <- data.frame(
    site_id,
    s_state,
    s_postcode,
    dc,
    ac,
    pv_installation_year_month,
    manufacturer,
    model,
    stringsAsFactors = FALSE
  )
  expect_error(process_raw_site_details(test_site_details))
})

test_that("Test the preprocessing of the site_details data", {
  # Test input data
  site_id <- c(101, 101, 300)
  s_state <- c("NSW", "NSW", "SA")
  s_postcode <- c(2031, 2031, 2031)
  dc <- c(60, 60, 10)
  ac <- c(59, 60, 8)
  pv_installation_year_month <- c("2017-01", "2017-10", "2018-01")
  manufacturer <- c("SMA", "SMA", "ABB")
  model <- c("1", "2", "3")
  test_site_details <- data.frame(
    site_id,
    s_state,
    s_postcode,
    dc,
    ac,
    pv_installation_year_month,
    manufacturer,
    model,
    stringsAsFactors = FALSE
  )
  # Test output data
  site_id <- c(101, 300)
  s_state <- c("NSW", "SA")
  s_postcode <- c(2031, 2031)
  sum_ac <- c(119, 8)
  first_ac <- c(59, 8)
  pv_installation_year_month <- c("2017-01", "2018-01")
  manufacturer <- c("SMA SMA", "ABB")
  model <- c("1 2", "3")
  expected_answer <- data.frame(
    site_id,
    s_state,
    pv_installation_year_month,
    sum_ac,
    first_ac,
    s_postcode,
    manufacturer,
    model,
    stringsAsFactors = FALSE
  )
  # Call processing function
  processed_site_details = process_raw_site_details(test_site_details)
  # Test the answer matches the expected answer
  expect_identical(processed_site_details, expected_answer)
})

test_that("Test the power calculations", {
  # Test input data
  e <- c(58.5, 102.5, 300, 40, 56, 80)
  polarity <- c(1, 1, 1, -1, -1, -1)
  d <- c(60, 30, 5, 60, 30, 5)
  test_combined_data <- data.frame(e, polarity, d, stringsAsFactors = FALSE)
  # Test output data
  e <- c(58.5, 102.5, 300, 40, 56, 80)
  polarity <- c(1, 1, 1, -1, -1, -1)
  d <- c(60, 30, 5, 60, 30, 5)
  e_polarity <- c(58.5, 102.5, 300, -40, -56, -80)
  power_kW <- c(0.00097500, 0.00341667, 0.06000000, -0.00066667, -0.00186667, -0.01600000)
  expected_answer <- data.frame(e, polarity, d, e_polarity, power_kW, stringsAsFactors = FALSE)
  # Call processing function
  test_combined_data = perform_power_calculations(test_combined_data)
  # Test the answer matches the expected answer
  expect_equal(test_combined_data, expected_answer, tolerance = 0.0001)
})

test_that("Test the standard categorisation function", {
  # Test input data
  site_id <- c(101, 50, 10002, 89, 567, 111, 1, 88, 4, 155, 40)
  s_state <- c("NSW", "NSW", "SA", "VIC", "QLD", "SA", "TAS", "SA", "SA", "QLD", "NSW")
  sum_dc <- c(60, 60, 10, 60, 60, 10, 11, 12, 20, 5, 6)
  first_ac <- c(60, 60, 10, 60, 60, 10, 11, 12, 20, 5, 6)
  pv_installation_year_month <- c(
    "",
    NA,
    "2015-01",
    "2015-10-09",
    "2015-11",
    "2016-10-09",
    "2017-10",
    "2020-10",
    "2021-02",
    "2022-01-14",
    "2023-09-12"
  )
  test_site_details <- data.frame(
    site_id,
    s_state,
    sum_dc,
    first_ac,
    pv_installation_year_month,
    stringsAsFactors = FALSE
  )
  # Test output data
  site_id <- c(101, 50, 10002, 89, 567, 111, 1, 88, 4, 155, 40)
  s_state <- c("NSW", "NSW", "SA", "VIC", "QLD", "SA", "TAS", "SA", "SA", "QLD", "NSW")
  sum_dc <- c(60, 60, 10, 60, 60, 10, 11, 12, 20, 5, 6)
  first_ac <- c(60, 60, 10, 60, 60, 10, 11, 12, 20, 5, 6)
  pv_installation_year_month <- c(
    ymd("2015-11-28"),
    ymd("2015-11-28"),
    ymd("2015-01-28"),
    ymd("2015-10-09"),
    ymd("2015-11-28"),
    ymd("2016-10-09"),
    ymd("2017-10-28"),
    ymd("2020-10-28"),
    ymd("2021-02-28"),
    ymd("2022-01-14"),
    ymd("2023-09-12")
  )
  Standard_Version <- c(
    "AS4777.3:2005",
    "AS4777.3:2005",
    "AS4777.3:2005",
    "AS4777.3:2005",
    "AS4777.3:2005",
    "AS4777.3:2005",
    "AS4777.2:2015",
    "AS4777.2:2015",
    "AS4777.2:2015",
    "AS4777.2:2015",
    "AS4777.2:2020 (>=Apr'23)"
  )
  expected_answer <- data.frame(
    site_id,
    s_state,
    sum_dc,
    first_ac,
    pv_installation_year_month,
    Standard_Version,
    stringsAsFactors = FALSE
  )
  # Call processing function
  processed_site_details = site_categorisation(test_site_details)
  # Test the answer matches the expected answer
  expect_identical(processed_site_details, expected_answer)
})

test_that("Power calculations are correct, example with different durations and polarities", {
  e <- c(10.0, -9.0, 11.5, -10.0, 9.0, 11.5)
  polarity <- c(1, 1, 1, -1, -1, -1)
  d <- c(5, 30, 60, 5, 30, 60)
  test_energy_data <- data.frame(e, polarity, d, stringsAsFactors = FALSE)
  e <- c(10.0, -9.0, 11.5, -10.0, 9.0, 11.5)
  polarity <- c(1, 1, 1, -1, -1, -1)
  d <- c(5, 30, 60, 5, 30, 60)
  e_polarity <- c(10.0, -9.0, 11.5, 10.0, -9.0, -11.5)
  power_kW <- c(0.002, -0.0003, 0.00019166666, 0.002, -0.0003, -0.00019166666)
  expected_energy_data <- data.frame(e, polarity, d, e_polarity, power_kW, stringsAsFactors = FALSE)
  expect_equal(perform_power_calculations(test_energy_data), expected_energy_data, tolerance = 0.0001)
})

test_that("Test assertion in install data processing functions triggers on incorrecttly formatted dates", {
  index <- c("2015-09-01", "2017-01-02", "2017/01/03")
  State <- c("NSW", "NSW", "NSW")
  Grouping <- c("<30 kW", "<30 kW", "<30 kW")
  Capacity <- c(100, 101, 102)
  install_data <- data.frame(index, State, Grouping, Capacity, stringsAsFactors = FALSE)
  expect_error(process_install_data(install_data))
})

test_that("Test assertion in install data processing functions triggers on unknown grouping cats", {
  index <- c("2015-09-01", "2017-01-02", "2017-01-03")
  State <- c("NSW", "NSW", "NSW")
  Grouping <- c("<30 kW", "<30 kW", "30-100 kW")
  Capacity <- c(100, 101, 102)
  install_data <- data.frame(index, State, Grouping, Capacity, stringsAsFactors = FALSE)
  expect_error(process_install_data(install_data))
})

test_that("Test assertion in install data processing functions triggers on non-numeric capacity values", {
  index <- c("2015-09-01", "2017-01-02", "2017-01-03")
  State <- c("NSW", "NSW", "NSW")
  Grouping <- c("<30 kW", "<30 kW", "30-100kW")
  Capacity <- c(100, 101, "x")
  install_data <- data.frame(index, State, Grouping, Capacity, stringsAsFactors = FALSE)
  expect_error(process_install_data(install_data))
})

test_that("Test assertion in install data processing functions triggers on unexpected state values", {
  index <- c(
    "2015-09-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01"
  )
  State <- c("NSW", "VIC", "SA", "TAS", "QLD", "NT", "ACT", "x")
  Grouping <- c("<30 kW", "<30 kW", "30-100kW", "<30 kW", "<30 kW", "30-100kW", "<30 kW", "<30 kW")
  Capacity <- c(100, 101, 100, 100, 101, 100, 100, 101)
  install_data <- data.frame(index, State, Grouping, Capacity, stringsAsFactors = FALSE)
  expect_error(process_install_data(install_data))
})

test_that("Test assertion in install data processing functions triggers on date with no preceding transition period", {
  index <- c(
    "2015-10-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01",
    "2017-01-01"
  )
  State <- c("NSW", "VIC", "SA", "TAS", "QLD", "NT", "ACT", "WA")
  Grouping <- c("<30 kW", "<30 kW", "30-100kW", "<30 kW", "<30 kW", "30-100kW", "<30 kW", "<30 kW")
  Capacity <- c(100, 101, 100, 100, 101, 100, 100, 101)
  install_data <- data.frame(index, State, Grouping, Capacity, stringsAsFactors = FALSE)
  expect_error(process_install_data(install_data))
})

test_that("Install data processing works on simple case", {
  date <- c(
    "2015-09-01",
    "2023-10-02",
    "2017-01-01",
    "2015-09-01",
    "2023-10-02",
    "2017-01-01",
    "2015-09-01",
    "2023-10-02",
    "2017-01-01"
  )
  state <- c("NSW", "NSW", "NSW", "NSW", "NSW", "NSW", "SA", "SA", "SA")
  sizegroup <- c("<30 kW", "<30 kW", "<30 kW", "30-100kW", "30-100kW", "30-100kW", "<30 kW", "<30 kW", "<30 kW")
  capacity <- c(100, 150, 210, 50, 100, 170, 60, 70, 80)
  install_data <- data.frame(date, state, sizegroup, capacity, stringsAsFactors = FALSE)
  date <- c(
    "2015-09-01",
    "2023-10-02",
    "2017-01-01",
    "2015-09-01",
    "2023-10-02",
    "2017-01-01",
    "2015-09-01",
    "2023-10-02",
    "2017-01-01"
  )
  s_state <- c("NSW", "NSW", "NSW", "NSW", "NSW", "NSW", "SA", "SA", "SA")
  Grouping <- c("<30 kW", "<30 kW", "<30 kW", "30-100kW", "30-100kW", "30-100kW", "<30 kW", "<30 kW", "<30 kW")
  Capacity <- c(100, 150, 210, 50, 100, 170, 60, 70, 80)
  date <- ymd(
    c(
      "2015-09-01",
      "2023-10-02",
      "2017-01-01",
      "2015-09-01",
      "2023-10-02",
      "2017-01-01",
      "2015-09-01",
      "2023-10-02",
      "2017-01-01"
    )
  )
  Standard_Version <- c(
    "AS4777.3:2005",
    "AS4777.2:2020 (>=Apr'23)",
    "AS4777.2:2015",
    "AS4777.3:2005",
    "AS4777.2:2020 (>=Apr'23)",
    "AS4777.2:2015",
    "AS4777.3:2005",
    "AS4777.2:2020 (>=Apr'23)",
    "AS4777.2:2015"
  )
  initial_cap <- c(0, 100, 150, 0, 50, 100, 0, 60, 70)
  standard_capacity <- c(100, 50, 60, 50, 50, 70, 60, 10, 10)
  expected_answer <- data.frame(
    date,
    s_state,
    Grouping,
    Capacity,
    Standard_Version,
    initial_cap,
    standard_capacity,
    stringsAsFactors = FALSE
  )
  answer <- process_install_data(install_data)
  expect_identical(answer, expected_answer)
})

test_that("Test assertion of s_postcode data assumptions, postcode can be interpreted as numeric", {
  # Test input data
  postcode <- c("101", "101", "x")
  lat <- c("1015.0", "1", "5")
  lon <- c("60", "60", "1")
  postcode_data <- data.frame(postcode, lat, lon, stringsAsFactors = FALSE)
  # Test output data
  expect_error(process_postcode_data(postcode_data))
})

test_that("Test assertion of s_postcode data assumptions, lat can be interpreted as numeric", {
  # Test input data
  postcode <- c("101", "101", "1")
  lat <- c("1015.0", "1", "x")
  lon <- c("60", "60", "1")
  postcode_data <- data.frame(postcode, lat, lon, stringsAsFactors = FALSE)
  # Test output data
  expect_error(process_postcode_data(postcode_data))
})

test_that("Test assertion of s_postcode data assumptions, lon can be interpreted as numeric", {
  # Test input data
  postcode <- c("101", "101", "1")
  lat <- c("1015.0", "1", "1")
  lon <- c("60", "", "1")
  postcode_data <- data.frame(postcode, lat, lon, stringsAsFactors = FALSE)
  # Test output data
  expect_error(process_postcode_data(postcode_data))
})

load_test_df <- function(text) {
  text <- gsub(" ", "", text)
  text <- gsub("~", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Calculating time offsets", {
  time_series_data <- "                 ts, c_id,  d
                       2018-01-01~00:01:05,    1,  5
                       2018-01-01~00:02:40,    2,  5
                       2018-01-01~00:03:33,    3, 30
                       2018-01-01~00:04:55,    4, 60
                       2018-01-01~00:04:40,    4, 60"

  time_series_data <- load_test_df(time_series_data)
  time_series_data <- mutate(time_series_data, ts = fastPOSIXct(ts, tz = "Australia/Brisbane"))

  data_with_offsets <- "                 ts, c_id,  d, time_offset
                        2018-01-01~00:01:05,    1,  5,           0
                        2018-01-01~00:02:40,    2,  5,           0
                        2018-01-01~00:03:33,    3, 30,           3
                        2018-01-01~00:04:55,    4, 60,          55
                        2018-01-01~00:04:40,    4, 60,          40"

  data_with_offsets <- load_test_df(data_with_offsets)
  data_with_offsets <- mutate(data_with_offsets, ts = fastPOSIXct(ts, tz = "Australia/Brisbane"))

  time_series_data <- get_time_offsets(time_series_data)

  testthat::expect_equal(time_series_data, data_with_offsets, tolerance = 1e-4)
})
