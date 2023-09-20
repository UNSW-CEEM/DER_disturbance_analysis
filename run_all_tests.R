# Automatically run all tests
# Stop on failure is set to TRUE by default. For local runs this can be set to
# FALSE to show full set of results.
STOP_ON_FAILURE <- TRUE

library(testthat)
source("load_tool_environment.R")
test_dir("db_interface/tests", stop_on_failure = STOP_ON_FAILURE)
test_dir("island_assessment/tests", stop_on_failure = STOP_ON_FAILURE)
test_dir("location_analysis/tests", stop_on_failure = STOP_ON_FAILURE)
test_dir("preprocess_cer_data/tests", stop_on_failure = STOP_ON_FAILURE)
test_dir("process_input_data/tests", stop_on_failure = STOP_ON_FAILURE)
test_dir("reconnect_compliance/tests", stop_on_failure = STOP_ON_FAILURE)
test_dir("response_categorisation/tests", stop_on_failure = STOP_ON_FAILURE)
test_dir("ufls_detection/tests", stop_on_failure = STOP_ON_FAILURE)
test_dir("upscale_disconnections/tests", stop_on_failure = STOP_ON_FAILURE)
test_dir("upscaling/tests", stop_on_failure = STOP_ON_FAILURE)
