# To use this file, run it as an R script
# If you are not running this from the tool top level directory ensure that you have set tool_directory to the root
# directory of the DER tool repository'
# Ensure that the output database is set to "ref" if building reference DBs, or "test" if building test DBs

library("logging")
library("rjson")
logging::basicConfig()

base_directory_name <- basename(getwd())
if (base_directory_name == "DER_disturbance_analysis") {
    tool_directory <- getwd()
} else {
    print("Script is not being run in DER_disturbance_analysis folder, make sure that tool directory ahs been set")
    tool_directory <- "~/UNSW/MATCH/DER_disturbance_analysis"
}
source(sprintf("%s/BDInterface/interface.R", tool_directory))

data_dirs <- list.dirs('validation/data', recursive=FALSE)
output_database <- "ref" # "test"
required_file_names <- c("ref_circuit_details.csv", "ref_meta_data.json", "ref_raw_data.csv", "ref_site_details.csv")

if (length(data_dirs) > 0){
    for (dir in data_dirs){
        all_files_in_dir <- list.files(dir)
        required_files_in_dir <- required_file_names %in% all_files_in_dir
        if (all(required_files_in_dir)){
            site_details_path_name <- paste(dir, "/", "ref_site_details.csv", sep="")
            circuit_details_path_name <- paste(dir, "/", "ref_circuit_details.csv", sep="")
            timeseries_path_name <- paste(dir, "/", "ref_raw_data.csv", sep="")
            metadata_path_name <- paste(dir, "/", output_database, "_meta_data.json", sep="")
            db_path_name <- paste(dir, "/", output_database, ".db", sep="")

            db <- DBInterface$new()
            if (!file.exists(db_path_name)){
                db$connect_to_new_database(db_path_name)
                logging::loginfo(paste("Creating new database", db_path_name))
            } else {
                db$connect_to_existing_database(db_path_name)
                logging::loginfo(paste("Replacing existing database", db_path_name))
            }

            db$default_timeseries_column_aliases <- list(utc_tstamp='_ts', c_id='_c_id', voltage='_v', frequency='_f', energy='_e',
                                                         duration='_d', power='_p', vmin='vmin', vmax='vmax',
                                                         vmean='vmean')
            db$build_database(timeseries = timeseries_path_name,
                              circuit_details = circuit_details_path_name,
                              site_details = site_details_path_name)

            db$add_postcode_lon_lat_to_database(sprintf("%s/inbuilt_data/postcode_lon_lat.csv", tool_directory))

            db$add_manufacturer_mapping_table(sprintf("%s/inbuilt_data/manufacturer_mapping.csv", tool_directory))

            db$run_data_cleaning_loop(500)

            # update metadata
            if (file.exists(metadata_path_name)){
                metadata <- rjson::fromJSON(file=metadata_path_name)
                metadata$database_name <- sprintf("%s/validation/%s/%s.db", tool_directory, dir, output_database)
                metadata_conn <- file(metadata_path_name)
                writeLines(rjson::toJSON(metadata, indent=4), metadata_conn)
                close(metadata_conn)
            }
        } else {
            logging::logerror(sprintf("Required files missing from directory: %s/%s", dir, required_file_names[!required_files_in_dir]))
        }
    }
} else {
    logging::logerror("No data found in directory")
}
