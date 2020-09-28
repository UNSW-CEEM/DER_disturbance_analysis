setwd(dirname(parent.frame(2)$ofile))
source("R/interface.R")
timeseries_path_name <- "tests/testthat/data/simple_timeseries.csv"
site_details_path_name <- "tests/testthat/data/simple_site_details.csv"
circuit_details_path_name <- "tests/testthat/data/simple_circuit_details.csv"
dp <- DBInterface$new()
dp$connect_to_new_database("example_database.db")
dp$build_database(timeseries=timeseries_path_name,
                  circuit_details=circuit_details_path_name,
                  site_details=site_details_path_name)
#browser()
#dp$get_filtered_time_series_data(state='NSW', duration=5, start_time='2018-01-01 00:00:20', 
#                                 end_time='2018-01-01 00:00:30')