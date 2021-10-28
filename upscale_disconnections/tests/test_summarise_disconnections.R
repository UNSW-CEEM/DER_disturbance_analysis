
testthat::context("Testing summarising disconnections.")


load_test_file <- function(text){
  text <- gsub(" ", "", text)
  text <- gsub("-", " ", text)
  df <- read.table(text = text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

testthat::test_that("group_disconnections_by_manufacturer",{
  
  circuit_summary <- "c_id, Standard_Version, manufacturer, response_category
                         1,                y,          SMA, 6-Not-enough-data
                         2,                y,          SMA,         Undefined
                         3,                y,          SMA,              blah
                         4,                y,          SMA,    3-Drop-to-Zero
                         5,                y,          SMA,      4-Disconnect
                         6,                y,            x,      4-Disconnect
                         7,                y,            x, 6-Not-enough-data
                         8,                z,            x,              blah
                         9,                z,            x,              blah
                        10,                z,            x,              blah"
  
  expected_output <- "Standard_Version, manufacturer, disconnections, sample_size
                                     y,          SMA,              2,           3
                                     y,            x,              1,           1
                                     z,            x,              0,           3" 

  circuit_summary <- load_test_file(circuit_summary)
  expected_output <- load_test_file(expected_output)
  output <- group_disconnections_by_manufacturer(circuit_summary)
  testthat::expect_equal(output, expected_output, tolerance = 1e-4)
})

testthat::test_that("join_circuit_summary_and_cer_manufacturer_data",{

  
  disconnection_summary <- "Standard_Version, manufacturer, disconnections, sample_size
                                           y,          SMA,              2,           3
                                           y,            x,              1,           1
                                           z,            x,              0,           3"
  
  cer_data <- "Standard_Version, manufacturer, capacity
                              y,          SMA,       10
                              y,            a,       11
                              z,            x,       12"
  
  expected_output <- "Standard_Version, manufacturer, disconnections, sample_size, cer_capacity
                                     y,            a,             NA,          NA,          11
                                     y,          SMA,              2,           3,          10
                                     y,            x,              1,           1,          NA
                                     z,            x,              0,           3,          12"
  
  disconnection_summary <- load_test_file(disconnection_summary)
  cer_data <- load_test_file(cer_data)
  expected_output <- load_test_file(expected_output)
  output <- join_circuit_summary_and_cer_manufacturer_data(disconnection_summary, cer_data)
  testthat::expect_equal(output, expected_output, tolerance = 1e-4)
})

testthat::test_that("impose_sample_size_threshold",{
  
  
  input <- "Standard_Version, manufacturer, disconnections, sample_size, cer_capacity
                           y,        Other,             NA,          NA,           11
                           y,          SMA,              2,           3,           10
                           y,      Mutiple,              1,           1,           NA
                           y,       Unkown,              1,           1,           NA
                           y,        Other,              1,           1,           NA
                           z,            x,              0,          30,           12"
  
  
  expected_output <- "Standard_Version, manufacturer, disconnections, sample_size, cer_capacity, proportion
                                     y,        Other,              5,           6,           21,  0.8333333
                                     z,            x,              0,          30,           12,          0"
  
  input <- load_test_file(input)
  expected_output <- load_test_file(expected_output)
  output <- impose_sample_size_threshold(input, sample_threshold = 30)
  expected_output <- mutate(expected_output, sample_size = as.numeric(sample_size))
  testthat::expect_equal(output, expected_output, tolerance = 1e-4)
})

testthat::test_that("calc_confidence_intervals_for_disconnections",{
  
  
  input <- "Standard_Version, manufacturer, disconnections, sample_size, cer_capacity, proportion
                           y,        Other,              5,           6,           21,  0.8333333
                           z,            x,              0,          30,           12,          0"
  
  expected_output <- "Standard_Version, manufacturer, disconnections, sample_size, cer_capacity, proportion, lower_bound, upper_bound
                                     y,        Other,              5,           6,           21,  0.8333333,   0.3583333,   0.9963333
                                     z,            x,              0,          30,           12,          0,         0.0,   0.1160000"
  
  input <- load_test_file(input)
  expected_output <- load_test_file(expected_output)
  output <- calc_confidence_intervals_for_disconnections(input)
  testthat::expect_equal(output, expected_output, tolerance = 1e-4)
})

testthat::test_that("calc_upscale_kw_loss",{
  
  input <- "Standard_Version, manufacturer, disconnections, sample_size, cer_capacity, proportion, lower_bound, upper_bound
                           y,        Other,              5,           6,           21,  0.8333333,   0.3583333,    0.996333  
                           z,            x,              0,          30,           12,          0,         0.0,   0.1160000"
  
  expected_output <- "Standard_Version, manufacturer, disconnections, sample_size, cer_capacity, proportion, lower_bound, upper_bound, predicted_kw_loss, lower_bound_kw_loss, upper_bound_kw_loss
                                     y,        Other,              5,           6,           21,  0.8333333,   0.3583333,   0.9963333,              17.5,            7.524999,              20.923
                                     z,            x,              0,          30,           12,          0,         0.0,   0.1160000,               0.0,                 0.0,               1.392"            
  
  input <- load_test_file(input)
  expected_output <- load_test_file(expected_output)
  output <- calc_upscale_kw_loss(input)
  testthat::expect_equal(output, expected_output, tolerance = 1e-4)
})