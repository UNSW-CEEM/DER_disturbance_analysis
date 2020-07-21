setwd(dirname(parent.frame(2)$ofile))
source("../R/interface.R")

timeseries_path_name <- "../../data/20200131/20200131_data.csv"
site_details_path_name <- "../../data/20200131/20200131_site_details_nem_new_names.csv"
circuit_details_path_name <- "../../data/20200131/20200131_circuit_details_nem.csv"

if (file.exists("2020_01_31.db")){file.remove("2020_01_31.db")}
db <- DBInterface$new()
db$connect_to_new_database("2020_01_31.db")
db$default_timeseries_column_aliases <- list(utc_tstamp='_ts', c_id='_c_id', voltage='_v', frequency='_f', energy='_e', 
                                             duration='_d', power='_p')
start_time <- Sys.time()
db$add_postcode_lon_lat_to_database("testthat/data/postcode_lon_lat.csv")
db$build_database(timeseries = timeseries_path_name,
                  circuit_details = circuit_details_path_name,
                  site_details = site_details_path_name)
end_time <- Sys.time()
print(end_time - start_time)
start_time <- Sys.time()
db$run_data_cleaning_loop(500)
end_time <- Sys.time()
print(end_time - start_time)