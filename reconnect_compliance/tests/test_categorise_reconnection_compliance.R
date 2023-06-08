
testthat::context("Testing categorising reconnections.")


load_test_df_keep_NA <- function(text) {
  text <- gsub(" ", "", text)
  text <- gsub("-", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "NNN")
  return(df)
}

load_test_df <- function(text) {
  text <- gsub(" ", "", text)
  text <- gsub("-", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("Calculating categorisation works",{
  
  reconnection_times <- "c_id,  response_category, ramp_above_threshold
                            1,       4-Disconnect,                 0.51
                            2,     3-Drop-to-Zero,                 0.50
                            3,       4-Disconnect,                 0.30
                            4,     3-Drop-to-Zero,                 0.29
                            5,              other,                   NA
                            6,     3-Drop-to-Zero,                   NA"
  
  reconnection_times <- load_test_df(reconnection_times)
  
  reconnection_times <- categorise_reconnection_compliance(reconnection_times,
                                                           ramp_threshold_for_compliance = 0.3,
                                                           ramp_threshold_for_non_compliance = 0.5)
  
  expected_categories <- "c_id, reconnection_compliance_status
                             1,                  Non-Compliant 
                             2,                         Unsure
                             3,                         Unsure
                             4,                      Compliant
                             5,                            NNN
                             6,                  Cannot-be-set"
  
  expected_categories <- load_test_df_keep_NA(expected_categories)
  testthat::expect_equal(reconnection_times, expected_categories, tolerance = 1e-4)
})