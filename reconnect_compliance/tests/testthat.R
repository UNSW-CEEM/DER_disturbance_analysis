library(testthat)
library(lubridate)
library(dplyr)
setwd(dirname(parent.frame(2)$ofile))
source("../R/specified_reconnect_profile.R")
source("../R/calculate_reconnection_times.R")
source("../R/categorise_reconnection_compliance.R")
test_dir("testthat")


