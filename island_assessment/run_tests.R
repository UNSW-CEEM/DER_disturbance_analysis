library(testthat)
source("load_tool_environment.R")
source("island_assessment/island_assessment_functions.R")
testthat::test_dir("island_assessment/tests")
