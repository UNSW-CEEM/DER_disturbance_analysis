setwd(dirname(parent.frame(2)$ofile))
source("../R/interface.R")

timeseries_path_name <- "../../data/2018-08-25/2018-05-25_sa_qld_fault_aemo.csv"
site_details_path_name <- "../../data/2018-08-25/sites_details_test.csv"
circuit_details_path_name <- "../../data/2018-08-25/circuit_details.csv"

if (file.exists("20180825.db")){file.remove("20180825.db")}
db <- DBInterface$new()
db$connect_to_new_database("20180825.db")
start_time <- Sys.time()
db$add_postcode_lon_lat_to_database("testthat/data/postcode_lon_lat.csv")
db$build_database(timeseries=timeseries_path_name,
                  circuit_details=circuit_details_path_name,
                  site_details=site_details_path_name)
end_time <- Sys.time()
print(end_time - start_time)
start_time <- Sys.time()
db$run_data_cleaning_loop(500)
end_time <- Sys.time()
print(end_time - start_time)