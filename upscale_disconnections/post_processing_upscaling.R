("load_tool_environment.R")

exclude_solar_edge <- TRUE
region_to_load <- 'SA'
load_start_time <- '2021-01-24'
circuit_summary <- read.csv(file = "data/2021-01-24/check_against_elisha/circuits_60s.csv", header = TRUE, stringsAsFactors = FALSE)
manufacturer_install_data <- read.csv(file = "inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv", header = TRUE, stringsAsFactors = FALSE)

circuits_to_summarise <- mutate(
  circuits_to_summarise,  manufacturer=ifelse(is.na(manufacturer), 'Other', manufacturer))
manufacturer_install_data <- mutate(
  manufacturer_install_data,  manufacturer=ifelse(is.na(manufacturer), 'Other', manufacturer))


manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)

if (exclude_solar_edge){
  circuits_to_summarise <- filter(circuit_summary, manufacturer != "SolarEdge")
  manufacturer_install_data <- filter(manufacturer_install_data, manufacturer != "SolarEdge")
} else {
  circuits_to_summarise <- circuit_summary
  manufacturer_install_data <- manufacturer_install_data
}

disconnection_summary <- group_disconnections_by_manufacturer(circuits_to_summarise)
manufacturer_capacitys <- get_manufacturer_capacitys(manufacturer_install_data, load_start_time, 
                                                     region_to_load)
disconnection_summary <- join_circuit_summary_and_cer_manufacturer_data(disconnection_summary,
                                                                        manufacturer_capacitys)
manufacters_missing_from_cer <- get_manufactures_in_input_db_but_not_cer(disconnection_summary)
manufacters_missing_from_input_db <- get_manufactures_in_cer_but_not_input_db(disconnection_summary)
disconnection_summary <- impose_sample_size_threshold(disconnection_summary, sample_threshold = 30)
disconnection_summary <- calc_confidence_intervals_for_disconnections(disconnection_summary)
disconnection_summary <- calc_upscale_kw_loss(disconnection_summary)
upscaled_disconnections <- upscale_disconnections(disconnection_summary)

write.csv(disconnection_summary, "test_disconnection_summary.csv", row.names = FALSE)
write.csv(upscaled_disconnections, "test_upscaled_disconnections.csv", row.names = FALSE)
write.csv(manufacters_missing_from_cer, "test_manufacters_missing_from_cer.csv", row.names=FALSE)
write.csv(manufacters_missing_from_input_db, "test_manufacters_missing_from_input_db.csv", row.names=FALSE)