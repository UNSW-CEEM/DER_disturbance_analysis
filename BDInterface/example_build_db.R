timeseries_path_name <- "data/2021-01-24/20210124_aemo.csv"
site_details_path_name <- "data/2021-01-24/20210124_site_details.csv"
circuit_details_path_name <- "data/2021-01-24/20210124_circuit_details.csv"

db <- DBInterface$new()

db$connect_to_new_database("data/2021-01-24/20210124_v2.db")

db$default_timeseries_column_aliases <- list(utc_tstamp='_ts', c_id='_c_id', voltage='_v', frequency='_f', energy='_e',
                                             duration='_d', power='_p', vmin='vmin', vmax='vmax',
                                             vmean='vmean')

db$build_database(timeseries = timeseries_path_name,
                  circuit_details = circuit_details_path_name,
                  site_details = site_details_path_name)

db$add_postcode_lon_lat_to_database("inbuilt_data/postcode_lon_lat.csv")

db$add_manufacturer_mapping_table("inbuilt_data/manufacturer_mapping.csv")

db$run_data_cleaning_loop(500)

