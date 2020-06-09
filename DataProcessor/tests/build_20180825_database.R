setwd(dirname(parent.frame(2)$ofile))
source("../R/interface.R")

timeseries_path_name <- "../../data/2018-08-25/2018-05-25_sa_qld_fault_aemo.csv"
site_details_path_name <- "../../data/2018-08-25/sites_details.csv"
circuit_details_path_name <- "../../data/2018-08-25/circuit_details.csv"

if (file.exists("20180825.db")){file.remove("20180825.db")}
dp <- DataProcessor$new()
dp$connect_to_new_database("20180825.db")
dp$build_database(timeseries=timeseries_path_name,
                  circuit_details=circuit_details_path_name,
                  site_details=site_details_path_name)