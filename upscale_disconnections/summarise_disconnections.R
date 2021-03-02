group_disconnections_by_manufacturer <- function(circuit_summary, cer_manufacturer_data, sample_threshold){
  
  # Don't count circuits without a well defined response type.
  bad_categories <- c("6 Not enough data", "Undefined")
  circuit_summary <- filter(circuit_summary, !(response_category %in% bad_categories | is.na(response_category)))
  
  # Get an initial summary of disconnection count and sample size by manufacturer.
  disconnection_summary <- group_by(circuit_summary, Standard_Version, manufacturer)
  disconnection_summary <- summarise(disconnection_summary, 
                                     disconnections = get_number_of_disconnections(response_category),
                                     sample_size = length(response_category))
  return(disconnection_summary)
}

get_number_of_disconnections <- function(response_categories){
  disconnection_categories <- c("3 Drop to Zero", "4 Disconnect")
  response_categories <- response_categories[response_categories %in% disconnection_categories]
  return(length(response_categories))
}

join_solar_analytics_and_cer_manufacturer_data <- function(circuit_summary, cer_manufacturer_data){
  circuit_summary <- merge(circuit_summary, cer_manufacturer_data, by = c('Standard_Version', 'manufacturer'), 
                           all = TRUE)
  circuit_summary <- mutate(circuit_summary, manufacturer = ifelse(is.na(capacity), 'Other', manufacturer))
  circuit_summary <- mutate(circuit_summary, manufacturer = ifelse(is.na(disconnections), 'Other', manufacturer))
  circuit_summary <- rename(circuit_summary, cer_capacity = capacity)
  return(circuit_summary)
}
  
impose_sample_size_threshold <- function(disconnection_summary, sample_threshold){
  # Create an Other group for manufacturers with a small sample size.
  disconnection_summary <- mutate(disconnection_summary, sample_size = ifelse(is.na(sample_size), 0, sample_size))
  disconnection_summary <- mutate(disconnection_summary, 
                                  manufacturer = ifelse(sample_size < sample_threshold | 
                                                          manufacturer == "Unknown" | 
                                                          manufacturer == "Multiple", 
                                                        "Other", manufacturer)
                                  )
  # Recalculate disconnection count and sample size. 
  disconnection_summary <- group_by(disconnection_summary, Standard_Version, manufacturer)
  disconnection_summary <- summarise(disconnection_summary, 
                                     disconnections = sum(disconnections, na.rm = TRUE),
                                     sample_size = sum(sample_size, na.rm = TRUE),
                                     cer_capacity = sum(cer_capacity, na.rm = TRUE))
  
  disconnection_summary <- mutate(disconnection_summary, proportion = disconnections / sample_size)
  disconnection_summary <- data.frame(disconnection_summary)
  return(disconnection_summary)
}

calc_confidence_intervals_for_disconnections <- function(disconnection_summary){
  result <- mapply(ConfidenceInterval, disconnection_summary$disconnections, 
                   disconnection_summary$sample_size, 0.95)
  disconnection_summary$lower <- result[1,]
  disconnection_summary$upper <- result[2,]
  disconnection_summary <- mutate(disconnection_summary, lower_error = proportion - lower, 
                                  upper_error = upper - proportion)
  return(disconnection_summary)
}



