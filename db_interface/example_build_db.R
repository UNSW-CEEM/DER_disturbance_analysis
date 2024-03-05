# timeseries_path_name <- "data/2021-01-24/20210124_aemo.csv"
# site_details_path_name <- "data/2021-01-24/20210124_site_details.csv"
# circuit_details_path_name <- "data/2021-01-24/20210124_circuit_details.csv"

timeseries_path_name <- "D:/DERDAT/DER_disturbance_analysis/data/Tesla_25_05_2021_data/30s_Tesla/ref_raw_data_30s_25052021_meantoright_roundts_v1.csv"
site_details_path_name <- "D:/DERDAT/DER_disturbance_analysis/data/Tesla_25_05_2021_data/30s_Tesla/site_details.csv"
circuit_details_path_name <- "D:/DERDAT/DER_disturbance_analysis/data/Tesla_25_05_2021_data/30s_Tesla/circuit_details.csv"


source("db_interface/interface.R")
db <- DBInterface$new()

db$connect_to_new_database("D:/DERDAT/DER_disturbance_analysis/data/25052021_30s_db_meantoright_roundts_v1.db")

db$default_timeseries_column_aliases <- list(
  utc_tstamp = "_ts",
  c_id = "_c_id",
  voltage = "_voltage",
  frequency = "_frequency",
  energy = "_e",
  duration = "_d",
  power = "_p",
  vmin = "_vmin",
  vmax = "_vmax",
  vmean = "_vmean"
)

db$build_database(
  timeseries = timeseries_path_name,
  circuit_details = circuit_details_path_name,
  site_details = site_details_path_name
)

db$add_postcode_lon_lat_to_database("inbuilt_data/postcode_lon_lat.csv")

db$add_manufacturer_mapping_table("inbuilt_data/manufacturer_mapping.csv")

db$run_data_cleaning_loop(500)
