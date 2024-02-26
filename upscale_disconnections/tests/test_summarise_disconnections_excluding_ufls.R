testthat::context("Testing summarising disconnections when UFLS circuits are removed from sample sizes.")


load_test_file <- function(text) {
  text <- gsub(" ", "", text)
  text <- gsub("-", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("group_disconnections_by_manufacturer", {
  circuit_summary <- "c_id, Standard_Version, manufacturer, response_category
                         1,                y,          SMA, 6-Not-enough-data
                         2,                y,          SMA,         Undefined
                         3,                y,          SMA,      UFLS-Dropout
                         4,                y,          SMA,    3-Drop-to-Zero
                         5,                y,          SMA,      4-Disconnect
                         6,                y,            x,      4-Disconnect
                         7,                y,            x, 6-Not-enough-data
                         8,                z,            x,              blah
                         9,                z,            x,              blah
                        10,                z,            x,      UFLS-Dropout"

  expected_output <- "Standard_Version, manufacturer, disconnections, sample_size
                                     y,          SMA,              2,           2
                                     y,            x,              1,           1
                                     z,            x,              0,           2"

  circuit_summary <- load_test_file(circuit_summary)
  expected_output <- load_test_file(expected_output)
  output <- group_disconnections_by_manufacturer(circuit_summary, exclude_ufls_circuits = TRUE)
  testthat::expect_equivalent(output, expected_output, tolerance = 1e-4)
})

testthat::test_that("get_number_of_ufls_disconnections", {
  circuit_summary <- "c_id, Standard_Version, manufacturer, response_category
                         1,                y,          SMA, 6-Not-enough-data
                         2,                y,          SMA,         Undefined
                         3,                y,          SMA,      UFLS-Dropout
                         4,                y,          SMA,    3-Drop-to-Zero
                         5,                y,          SMA,      4-Disconnect
                         6,                y,            x,      4-Disconnect
                         7,                y,            x, 6-Not-enough-data
                         8,                z,            x,              blah
                         9,                z,            x,              blah
                        10,                z,            x,      UFLS-Dropout"

  expected_output <- list(sample_size = 7, disconnections = 2)

  circuit_summary <- load_test_file(circuit_summary)
  output <- get_number_of_ufls_disconnections(circuit_summary$response_category)
  testthat::expect_equal(output, expected_output, tolerance = 1e-4)
})

testthat::test_that("scale_manufacturer_capacities_by_ufls", {
  circuit_summary <- "Standard_Version, manufacturer, disconnections, sample_size, s_state, cer_capacity
                                     y,            a,             NA,          NA,     QLD,           13
                                     y,          SMA,              2,           3,     QLD,           10
                                     y,            x,              1,           1,     QLD,           NA
                                     z,            x,              0,           3,     QLD,           108"

  ufls_stats <- list(sample_size = 6, disconnections = 3)

  expected_output <- "Standard_Version, manufacturer, disconnections, sample_size, s_state, cer_capacity
                                     y,            a,             NA,          NA,     QLD,          6.5
                                     y,          SMA,              2,           3,     QLD,            5
                                     y,            x,              1,           1,     QLD,           NA
                                     z,            x,              0,           3,     QLD,           54
  UFLS_disconnections_and_totals_including_ULFS_affected_circuits, UFLS, 3, 6, UFLS, 131"

  circuit_summary <- load_test_file(circuit_summary)
  expected_output <- load_test_file(expected_output)
  output <- scale_manufacturer_capacities_by_ufls(circuit_summary, ufls_stats)
  testthat::expect_equal(output, expected_output, tolerance = 1e-4)
})
