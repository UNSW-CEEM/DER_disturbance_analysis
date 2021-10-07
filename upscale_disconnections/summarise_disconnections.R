get_upscaling_results <- function(circuit_summary, manufacturer_install_data, event_date, region, sample_threshold){
  # Upscale the proportion of disconnecting circuits by manufacturer to better represent the installed capacity.
  out <- list()
  disconnection_summary <- group_disconnections_by_manufacturer(circuit_summary)
  manufacturer_capacitys <- get_manufacturer_capacitys(manufacturer_install_data, event_date, region)
  disconnection_summary <- join_circuit_summary_and_cer_manufacturer_data(disconnection_summary, manufacturer_capacitys)
  out$manufacturers_missing_from_cer <- get_manufactures_in_input_db_but_not_cer(disconnection_summary)
  out$manufacturers_missing_from_input_db <- get_manufactures_in_cer_but_not_input_db(disconnection_summary)
  disconnection_summary <- impose_sample_size_threshold(disconnection_summary, sample_threshold)
  disconnection_summary <- calc_confidence_intervals_for_disconnections(disconnection_summary)
  out$disconnection_summary <- calc_upscale_kw_loss(disconnection_summary)
  out$upscaled_disconnections <- upscale_disconnections(out$disconnection_summary)
  return(out)
}

get_upscaling_results_excluding_ufls <- function(circuit_summary, manufacturer_install_data, event_date, region, 
                                                 sample_threshold){
  # Upscale the proportion of disconnecting circuits based on sample sizes once UFLS circuits are removed.
  out <- list()
  disconnection_summary <- group_disconnections_by_manufacturer(circuit_summary, exclude_ufls_circuits = TRUE)
  ufls_stats <- get_number_of_ufls_disconnections(circuit_summary$response_category)
  manufacturer_capacitys <- get_manufacturer_capacitys(manufacturer_install_data, event_date, region)
  disconnection_summary <- join_circuit_summary_and_cer_manufacturer_data(disconnection_summary, manufacturer_capacitys)
  out$manufacturers_missing_from_cer <- get_manufactures_in_input_db_but_not_cer(disconnection_summary)
  out$manufacturers_missing_from_input_db <- get_manufactures_in_cer_but_not_input_db(disconnection_summary)
  disconnection_summary <- scale_manufacturer_capacities_by_ufls(disconnection_summary, ufls_stats)
  disconnection_summary <- impose_sample_size_threshold(disconnection_summary, sample_threshold)
  disconnection_summary <- calc_confidence_intervals_for_disconnections(disconnection_summary)
  disconnection_summary <- calc_upscale_kw_loss(disconnection_summary)
  upscaled_disconnections <- upscale_disconnections(disconnection_summary)
  disconnection_summary <- disconnection_summary %>% 
    rename(
      sample_size_after_removing_UFLS_affected_circuits = sample_size,
      cer_capacity_reduced_by_UFLS_proportion = cer_capacity
    )
  out$disconnection_summary <- disconnection_summary
  upscaled_disconnections <- upscaled_disconnections %>% 
    rename(
      sample_size_after_removing_UFLS_affected_circuits = sample_size,
      cer_capacity_reduced_by_UFLS_proportion = cer_capacity
    )
  out$upscaled_disconnections <- upscaled_disconnections
  return(out)
}

group_disconnections_by_manufacturer <- function(circuit_summary, exclude_ufls_circuits=FALSE){
  # Don't count circuits without a well defined response type.
  if (exclude_ufls_circuits) {
    bad_categories <- c("6 Not enough data", "Undefined", "UFLS Dropout")
  } else {
    bad_categories <- c("6 Not enough data", "Undefined")
  }
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

get_number_of_ufls_disconnections <- function(response_categories){
  # Find the number of circuits identified as UFLS Dropout and the sample size to use for the UFLS proportion
  ufls_stats <- list()
  bad_categories <- c("6 Not enough data", "Undefined")
  response_categories <- response_categories[!(response_categories %in% bad_categories | is.na(response_categories))]
  ufls_stats$sample_size <- length(response_categories)
  disconnection_categories <- c("UFLS Dropout")
  response_categories <- response_categories[response_categories %in% disconnection_categories]
  ufls_stats$disconnections <- length(response_categories)
  return(ufls_stats)
}

scale_manufacturer_capacities_by_ufls <- function(disconnection_summary, ufls_stats){
  # Reduce the CER capacities by the UFLS proportion
  ufls_proportion <- ufls_stats$disconnections / ufls_stats$sample_size
  disconnection_summary <- mutate(disconnection_summary, cer_capacity=cer_capacity*(1-ufls_proportion))
  # Add a UFLS row to the disconnection summary
  ufls_row <- data.frame(Standard_Version="UFLS_disconnections_and_totals_including_ULFS_affected_circuits", 
                         manufacturer="UFLS", disconnections=ufls_stats$disconnections, 
                         sample_size=ufls_stats$sample_size, s_state="UFLS", 
                         cer_capacity=sum(disconnection_summary$cer_capacity, na.rm = TRUE) / (1-ufls_proportion))
  disconnection_summary <- rbind(disconnection_summary, ufls_row)
  return(disconnection_summary)
}

join_circuit_summary_and_cer_manufacturer_data <- function(circuit_summary, cer_manufacturer_data){
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
                                                          manufacturer == "" |
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

get_manufactures_in_input_db_but_not_cer <- function(disconnection_summary){
  disconnection_summary <- filter(disconnection_summary, is.na(cer_capacity))
  return(disconnection_summary)
}

get_manufactures_in_cer_but_not_input_db <- function(disconnection_summary){
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



