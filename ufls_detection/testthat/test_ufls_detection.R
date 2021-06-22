testthat::context("Testing the ulfs detection function.")

load_test_df <- function(text){
  text <- gsub(" ", "", text)
  text <- gsub("-", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("test simple case no ufls dropout",{
  
  DBInterface <- R6::R6Class("DBInterface",
                             public = list(
                               get_samples_per_circuit = function(state, start_time, end_time){
                                 pre_event_samples <- " c_id, samples
                                                           1,      10"
                                 post_event_samples <- " c_id, samples
                                                            1,      6"
                                 if (start_time < '2021-01-01 13:00:00'){
                                   return(load_test_df(pre_event_samples))
                                 } else {
                                   return(load_test_df(post_event_samples))
                                 }
                               }
                             ))
  db <- DBInterface$new()
  
  ufls_statuses <- ufls_detection(db = db, region = 'blah', 
                                  pre_event_interval = strptime('2021-01-01 13:00:00', format="%Y-%m-%d %H:%M:%S", tz="GMT"), 
                                  event_window_length = 5, pct_sample_reduction_threshold = 0.5)
  
  expected_results <- "c_id, pre_event_count, post_event_count,      ufls_status
                          1,                10,              6, 'No-UFLS-Dropout'"
  
  expected_results <- load_test_df(expected_results)
  
  testthat::expect_equal(ufls_statuses, expected_results)
})