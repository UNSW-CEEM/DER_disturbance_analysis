library(testthat)
library(dplyr)
library(lubridate)
setwd(dirname(parent.frame(2)$ofile))
source("../summarise_disconnections.R")
source("../../Confidence Intervals Binomial.R")
test_dir("testthat")


