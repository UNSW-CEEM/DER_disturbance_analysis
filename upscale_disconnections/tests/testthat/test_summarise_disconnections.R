
testthat::context("Testing summarising disconnections.")


load_test_file <- function(text){
  text <- gsub(" ", "", text)
  text <- gsub("-", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("simple case",{
  
  circuit_summary <- "c_id, Standard_Version, manufacturer, response_category
                         1,                y,          SMA, 6-Not-enough-data
                         2,                y,          SMA,         Undefined
                         3,                y,          SMA,              blah
                         4,                y,          SMA,    3-Drop-to-Zero
                         5,                y,          SMA,      4-Disconnect
                         6,                y,            x,      4-Disconnect
                         7,                y,            x, 6-Not-enough-data
                         8,                y,            x,              blah
                         9,                y,      Mutiple,              blah
                        10,                y,      Unknown,              blah"
  
  expected_output <- "Standard_Version, manufacturer, disconnections, total
                                     y,          SMA,              2,     3
                                     y,       Other,              1,     4"

  circuit_summary <- load_test_file(circuit_summary)
  expected_output <- load_test_file(expected_output)
  browser()
  output <- summarise_disconnections(circuit_summary, sample_threshold = 3)
  testthat::expect_equal(output, expected_output, tolerance = 1e-4)
})