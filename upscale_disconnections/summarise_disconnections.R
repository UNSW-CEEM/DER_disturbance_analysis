group_disconnections_by_manufacturer <- function(circuit_summary){
  
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
  disconnection_summary$lower_bound <- result[1,]
  disconnection_summary$upper_bound <- result[2,]
  return(disconnection_summary)
}

calc_upscale_mw_loss <- function(disconnection_summary){
  disconnection_summary <- mutate(disconnection_summary, 
                                  predicted_mw_loss = proportion * cer_capacity,
                                  lower_bound_mw_loss = lower_bound * cer_capacity,
                                  upper_bound_mw_loss = upper_bound * cer_capacity)
  return(disconnection_summary)
}

get_manufactures_in_solar_analytics_but_not_cer <- function(disconnection_summary){
  disconnection_summary <- filter(disconnection_summary, is.na(cer_capacity))
  return(disconnection_summary)
}

get_manufactures_in_cer_but_not_solar_analytics <- function(disconnection_summary){
  disconnection_summary <- filter(disconnection_summary, is.na(sample_size))
  return(disconnection_summary)
}



