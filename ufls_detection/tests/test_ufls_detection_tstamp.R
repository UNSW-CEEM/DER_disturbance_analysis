testthat::context("Testing the ulfs detection function based on missing timestamps.")

load_test_df <- function(text) {
  text <- gsub(" ", "", text)
  text <- gsub("~", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("test bad circuit signal ufls no dropout", {
  DBInterface <- R6::R6Class(
    "DBInterface",
    public = list(
      get_filtered_time_series_data_all_durations = function(state, start_time, end_time) {
        pre_event_samples <- "                 ts, c_id,  d
                              2021-01-01~03:00:00,    1, 241
                              2021-01-01~03:00:00,    2, 241
                              2021-01-01~03:00:00,    3, 239"
        post_event_samples <- "                 ts, c_id, d
                               2021-01-01~03:01:00,    1, 5"
        comp_time <- format(
          strptime("2021-01-01 13:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane") - 0,
          tz = "GMT"
        )
        if (start_time < comp_time) {
          return(load_test_df(pre_event_samples))
        } else {
          return(load_test_df(post_event_samples))
        }
      }
    )
  )
  db <- DBInterface$new()

  ufls_statuses <- ufls_detection_tstamp(
    db = db,
    region = "blah",
    pre_event_interval = strptime("2021-01-01 13:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
    pre_event_window_length = 5,
    post_event_window_length = 9,
    pre_pct_sample_seconds_threshold = 0.8
  )

  expected_results <- "c_id, pre_event_sampled_seconds, post_event_sampled_seconds,         ufls_status
                          1,                       241,                          5,   'No~UFLS~Dropout'
                          2,                       241,                          0,      'UFLS~Dropout'
                          3,                       239,                          0,  'No~UFLS~Dropout'"
  expected_results <- load_test_df(expected_results)

  testthat::expect_equal(ufls_statuses, expected_results)
})

testthat::test_that("test simple case no ufls dropout", {
  DBInterface <- R6::R6Class(
    "DBInterface",
    public = list(
      get_filtered_time_series_data_all_durations = function(state, start_time, end_time) {
        pre_event_samples <- "                 ts, c_id,  d
                              2021-01-01~03:00:00,    1, 10"
        post_event_samples <- "                 ts, c_id, d
                               2021-01-01~03:01:00,    1, 5"
        comp_time <- format(
          strptime("2021-01-01 13:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane") - 0,
          tz = "GMT"
        )
        if (start_time < comp_time){
          return(load_test_df(pre_event_samples))
        } else {
          return(load_test_df(post_event_samples))
        }
      }
    )
  )
  db <- DBInterface$new()

  ufls_statuses <- ufls_detection_tstamp(
    db = db,
    region = "blah",
    pre_event_interval = strptime("2021-01-01 13:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
    pre_event_window_length = 5,
    post_event_window_length = 9,
    pre_pct_sample_seconds_threshold = 0.8
  )

  expected_results <- "c_id, pre_event_sampled_seconds, post_event_sampled_seconds,       ufls_status
                          1,                        10,                          5,  'No~UFLS~Dropout'"
  expected_results <- load_test_df(expected_results)

  testthat::expect_equal(ufls_statuses, expected_results)
})

testthat::test_that("test simple case ufls dropout", {
  DBInterface <- R6::R6Class(
    "DBInterface",
    public = list(
      get_filtered_time_series_data_all_durations = function(state, start_time, end_time) {
        pre_event_samples <- "                 ts, c_id,  d
                              2021-01-01~03:00:00,    1, 300
                              2021-01-01~03:00:00,    2, 300"
        post_event_samples <- "                 ts, c_id, d
                              2021-01-01~03:01:00,    1, 5"
        comp_time <- format(
          strptime("2021-01-01 13:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane") - 0,
          tz = "GMT"
        )
        if (start_time < comp_time) {
          return(load_test_df(pre_event_samples))
        } else {
          return(load_test_df(post_event_samples))
        }
      }
    )
  )
  db <- DBInterface$new()

  ufls_statuses <- ufls_detection_tstamp(
    db = db,
    region = "blah",
    pre_event_interval = strptime("2021-01-01 13:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
    pre_event_window_length = 5,
    post_event_window_length = 9,
    pre_pct_sample_seconds_threshold = 0.8
  )

  expected_results <- "c_id, pre_event_sampled_seconds, post_event_sampled_seconds,      ufls_status
                          1,                        300,                          5,   'No~UFLS~Dropout'
                          2,                        300,                          0,    'UFLS~Dropout'"

  expected_results <- load_test_df(expected_results)

  testthat::expect_equal(ufls_statuses, expected_results)
})

testthat::test_that("test sampled_time_per_circuit just one 5s data point", {
  ts_data <- "                 ts, c_id, d
              2021-01-01~03:00:00,    1, 5"

  ts_data <- load_test_df(ts_data)

  start_time <- strptime("2021-01-01 12:55:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  end_time <- strptime("2021-01-01 13:05:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")

  sampled_time <- calc_sampled_time_per_circuit(ts_data, start_time, end_time)

  expected_results <- "c_id, sampled_seconds
                          1,               5"

  expected_results <- load_test_df(expected_results)

  testthat::expect_equal(sampled_time, expected_results)
})

testthat::test_that("test sampled_time_per_circuit 5s and 60s data point", {
  ts_data <- "                 ts, c_id,  d
              2021-01-01~03:00:00,    1,  5
              2021-01-01~03:00:00,    1, 60"

  ts_data <- load_test_df(ts_data)

  start_time <- strptime("2021-01-01 12:55:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  end_time <- strptime("2021-01-01 13:05:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")

  sampled_time <- calc_sampled_time_per_circuit(ts_data, start_time, end_time)

  expected_results <- "c_id, sampled_seconds
                          1,              60"

  expected_results <- load_test_df(expected_results)

  testthat::expect_equal(sampled_time, expected_results)
})

testthat::test_that(
  "test sampled_time_per_circuit 5s and 60s data point and that time outsidte the window isn't counted.",
{
  ts_data <- "                 ts, c_id,  d
              2021-01-01~03:00:00,    1,  5
              2021-01-01~02:55:30,    1, 60"

  ts_data <- load_test_df(ts_data)

  start_time <- strptime("2021-01-01 12:55:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  end_time <- strptime("2021-01-01 13:05:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")

  sampled_time <- calc_sampled_time_per_circuit(ts_data, start_time, end_time)

  expected_results <- "c_id, sampled_seconds
                          1,              35"

  expected_results <- load_test_df(expected_results)

  testthat::expect_equal(sampled_time, expected_results)
})

testthat::test_that("test sampled_time_per_circuit two circuits.", {
  ts_data <- "                 ts, c_id,  d
              2021-01-01~03:00:00,    1,  5
              2021-01-01~02:55:30,    2, 60"

  ts_data <- load_test_df(ts_data)

  start_time <- strptime("2021-01-01 12:55:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")
  end_time <- strptime("2021-01-01 13:05:00", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane")

  sampled_time <- calc_sampled_time_per_circuit(ts_data, start_time, end_time)

  expected_results <- "c_id, sampled_seconds
                          1,               5
                          2,              30"

  expected_results <- load_test_df(expected_results)

  testthat::expect_equal(sampled_time, expected_results)
})
