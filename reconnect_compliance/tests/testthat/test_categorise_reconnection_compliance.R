
testthat::context("Testing categorising reconnections.")


load_test_df_keep_NA <- function(text){
  text <- gsub(" ", "", text)
  text <- gsub("-", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "NNN")
  return(df)
}

load_test_df <- function(text){
  text <- gsub(" ", "", text)
  text <- gsub("-", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Calculating categorisation works",{
  
  reconnection_times <- "c_id,  response_category, reconnection_time
                            1,       4-Disconnect,               1.0
                            2,     3-Drop-to-Zero,               2.0
                            3,       4-Disconnect,               5.0
                            4,     3-Drop-to-Zero,               5.0
                            5,              other,                NA
                            6,     3-Drop-to-Zero,                NA"
  
  reconnection_times <- load_test_df(reconnection_times)
  
  reconnection_times <- categorise_reconnection_compliance(reconnection_times, compliance_threshold_minutes = 3)
  
  expected_categories <- "c_id, reconnection_compliance_status
                             1,                       Too-fast
                             2,                       Too-fast
                             3,                      Compliant
                             4,                      Compliant
                             5,                             NA
                             6,                  Cannot-be-set"
  expected_categories <- load_test_df_keep_NA(expected_categories)
  testthat::expect_equal(reconnection_times, expected_categories, tolerance = 1e-4)
})