setwd(dirname(parent.frame(2)$ofile))
source("../R/interface.R")

timeseries_path_name <- "../../data/2019-11-26/nsw_qld_20191126_data.csv"
site_details_path_name <- "../../data/2019-11-26/site_details_nsw_qld.csv"
circuit_details_path_name <- "../../data/2019-11-26/circuit_details_nsw_qld.csv"

if (file.exists("20191126.db")){file.remove("20191126.db")}
db <- DBInterface$new()
db$default_timeseries_column_aliases <- list(utc_tstamp='_ts', c_id='_c_id', voltage='_v', 
                                             frequency='_f', energy='_e', duration='_d', power='_p')
db$connect_to_new_database("20191126.db")
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