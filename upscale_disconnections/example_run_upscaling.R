source("load_tool_environment.R")

exclude_solar_edge <- FALSE
region_to_load <- 'QLD'
load_start_time <- '2021-05-25'
circuit_summary <- read.csv(file = "circuit_summary.csv", header = TRUE, stringsAsFactors = FALSE)
manufacturer_install_data <- read.csv(file = "inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv", 
                                      header = TRUE, stringsAsFactors = FALSE)

if (exclude_solar_edge) {
  circuits_to_summarise <- filter(circuit_summary, manufacturer != "SolarEdge")
  manufacturer_install_data <- filter(manufacturer_install_data, manufacturer != "SolarEdge")
} else {
  circuits_to_summarise <- circuit_summary
  manufacturer_install_data <- manufacturer_install_data
}

manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)

upsc_results <- get_upscaling_results_excluding_ufls_affected_circuits(circuits_to_summarise, manufacturer_install_data,
                                                                       load_start_time, region_to_load, 30)

disconnection_summary <- upsc_results$disconnection_summary
upscaled_disconnections <- upsc_results$upscaled_disconnections
manufacturers_missing_from_cer <- upsc_results$manufacturers_missing_from_cer
manufacturers_missing_from_input_db <- upsc_results$manufacturers_missing_from_input_db

write.csv(disconnection_summary, "test_disconnection_summary.csv", row.names = FALSE)
write.csv(upscaled_disconnections, "test_upscaled_disconnections.csv", row.names = FALSE)
write.csv(manufacturers_missing_from_cer, "test_manufacturers_missing_from_cer.csv", row.names=FALSE)
write.csv(manufacturers_missing_from_input_db, "test_manufacturers_missing_from_input_db.csv", row.names=FALSE)