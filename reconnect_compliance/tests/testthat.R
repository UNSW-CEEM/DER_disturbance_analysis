library(testthat)
library(lubridate)
library(dplyr)
setwd(dirname(parent.frame(2)$ofile))
source("../R/specified_reconnect_profile.R")
source("../R/calculate_reconnection_times.R")
source("../R/calculate_max_ramp_rate.R")
source("../R/calculate_ramp_rates.R")
source("../R/find_first_resource_limited_interval.R")
source("../R/categorise_reconnection_compliance.R")
test_dir("testthat")


