library(testthat)
source("load_tool_environment.R")
testthat::test_dir("DBInterface/tests")
testthat::test_dir("location_analysis/tests")
