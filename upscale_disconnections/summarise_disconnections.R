get_upscaling_results <- function(circuit_summary, manufacturer_install_data, event_date, region, sample_threshold){
  out <- list()
  disconnection_summary <- group_disconnections_by_manufacturer(circuit_summary)
  manufacturer_capacitys <- get_manufacturer_capacitys(manufacturer_install_data, event_date, region)
  disconnection_summary <- join_solar_analytics_and_cer_manufacturer_data(disconnection_summary, manufacturer_capacitys)
  out$manufacters_missing_from_cer <- get_manufactures_in_solar_analytics_but_not_cer(disconnection_summary)
  out$manufacters_missing_from_input_db <- get_manufactures_in_cer_but_not_solar_analytics(disconnection_summary)
  disconnection_summary <- impose_sample_size_threshold(disconnection_summary, sample_threshold)
  disconnection_summary <- calc_confidence_intervals_for_disconnections(disconnection_summary)
  out$disconnection_summary <- calc_upscale_kw_loss(disconnection_summary)
  out$upscaled_disconnections <- upscale_disconnections(out$disconnection_summary)
  return(out)
}

group_disconnections_by_manufacturer <- function(circuit_summary){
  # Don't count circuits without a well defined response type.
  # TODO: include UFLS in bad categories
  bad_categories <- c("6 Not enough data", "Undefined")
  # TODO: | response_category == "NA"
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

# TODO: get_number_of_ufls_disconnections

join_solar_analytics_and_cer_manufacturer_data <- function(circuit_summary, cer_manufacturer_data){
  circuit_summary <- merge(circuit_summary, cer_manufacturer_data, by = c('Standard_Version', 'manufacturer'), 
                           all = TRUE)
  circuit_summary <- rename(circuit_summary, cer_capacity = capacity)
  return(circuit_summary)
}
  
impose_sample_size_threshold <- function(disconnection_summary, sample_threshold){
  circuit_summary <- mutate(disconnection_summary, sample_threshold, manufacturer = ifelse(is.na(cer_capacity), 
                                                                                           'Other', manufacturer))
  circuit_summary <- mutate(disconnection_summary, sample_threshold, manufacturer = ifelse(is.na(disconnections), 
                                                                                           'Other', manufacturer))
  
  # Create an Other group for manufacturers with a small sample size.
  disconnection_summary <- mutate(disconnection_summary, sample_size = ifelse(is.na(sample_size), 0, sample_size))
  disconnection_summary <- mutate(disconnection_summary, 
                                  manufacturer = ifelse(sample_size < sample_threshold | 
                                                          manufacturer == "Unknown" | 
                                                          manufacturer == "Multiple" |
                                                          manufacturer == "Mixed" | 
                                                          is.na(manufacturer), 
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
  result <- mapply(CI_wrapper, disconnection_summary$disconnections, 
                   disconnection_summary$sample_size, 0.95)
  disconnection_summary$lower_bound <- result[1,]
  disconnection_summary$upper_bound <- result[2,]
  return(disconnection_summary)
}

CI_wrapper <- function(count, sample_size, ci){
  if (sample_size > 0) {
    result <- ConfidenceInterval(count, sample_size, ci)
  } else {
    result <- c(NA, NA)
  }
  
  return(result)
}

calc_upscale_kw_loss <- function(disconnection_summary){
  disconnection_summary <- mutate(disconnection_summary, 
                                  predicted_kw_loss = proportion * cer_capacity,
                                  lower_bound_kw_loss = lower_bound * cer_capacity,
                                  upper_bound_kw_loss = upper_bound * cer_capacity)
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

get_manufacturer_capacitys <- function(manufacturer_install_data, event_date, region){
  manufacturer_install_data <- filter(manufacturer_install_data, s_state == region)
  manufacturer_install_data <- manufacturer_install_data[order(manufacturer_install_data$date), ]
  manufacturer_install_data <- filter(manufacturer_install_data, date <= event_date)
  manufacturer_install_data <- group_by(manufacturer_install_data, Standard_Version, manufacturer,  s_state)
  manufacturer_install_data <- summarise(manufacturer_install_data, capacity = last(standard_capacity))
  manufacturer_install_data <- as.data.frame(manufacturer_install_data)
  return(manufacturer_install_data)
}

upscale_disconnections <- function(manufacturer_level_summary){
  upscaled_disconnections <- group_by(manufacturer_level_summary, Standard_Version)
  upscaled_disconnections <- summarise(upscaled_disconnections,
                                       sample_size = sum(sample_size),
                                       unscaled_disconnections = sum(disconnections) / sum(sample_size),
                                       cer_capacity = sum(cer_capacity),
                                       lower_bound_kw_loss = sum(lower_bound_kw_loss),
                                       predicted_kw_loss = sum(predicted_kw_loss),
                                       upper_bound_kw_loss = sum(upper_bound_kw_loss),                         
                                       lower_bound = sum(lower_bound_kw_loss)/sum(cer_capacity),
                                       disconnections = sum(predicted_kw_loss)/sum(cer_capacity),
                                       uppper_bound = sum(upper_bound_kw_loss)/sum(cer_capacity))
  return(upscaled_disconnections)
}



