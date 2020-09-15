
catergorise_row <- function(response_category, 
                            reconnection_time,
                            max_reconnection_ramp_rate,
                            max_norm_power,
                            max_norm_output_threshold, 
                            reconnection_time_threshold_for_compliance,
                            reconnection_time_threshold_for_non_compliance,
                            ramp_rate_threshold_for_compliance,
                            ramp_rate_threshold_for_non_compliance){
  
  if (response_category %in% c('4 Disconnect', '3 Drop to Zero')){
    if (is.na(reconnection_time)) {
      category <- 'Cannot be set'
    } else {
      if(reconnection_time > reconnection_time_threshold_for_compliance 
                & max_reconnection_ramp_rate < ramp_rate_threshold_for_compliance){
        category <- 'Compliant'
      } else if(reconnection_time < reconnection_time_threshold_for_non_compliance 
                | max_reconnection_ramp_rate > ramp_rate_threshold_for_non_compliance) {
        category <- 'Non Compliant'
      } else {
        category <- 'Unsure'
      }
      if (max_norm_power > max_norm_output_threshold & category == 'Non Compliant') {
        category <- 'Unsure'
      }
    }
  } else {
    category <- "NA"
  }
  return(category)
}

categorise_reconnection_compliance <- function(reconnection_times,
                                               max_norm_output_threshold,
                                               reconnection_time_threshold_for_compliance,
                                               reconnection_time_threshold_for_non_compliance,
                                               ramp_rate_threshold_for_compliance,
                                               ramp_rate_threshold_for_non_compliance){
  
  reconnection_times <- rowwise(reconnection_times) %>% mutate(
    reconnection_compliance_status = catergorise_row(response_category, 
                                                     reconnection_time,
                                                     max_reconnection_ramp_rate,
                                                     max_norm_power,
                                                     max_norm_output_threshold, 
                                                     reconnection_time_threshold_for_compliance,
                                                     reconnection_time_threshold_for_non_compliance,
                                                     ramp_rate_threshold_for_compliance,
                                                     ramp_rate_threshold_for_non_compliance))
  
  reconnection_times <- select(reconnection_times, c_id, reconnection_compliance_status)
  return(reconnection_times)
}

catergorise_row_max_ramp <- function(response_category, max_ramp_rate, compliance_threshold_ramp_rate){
  if (response_category %in% c('4 Disconnect', '3 Drop to Zero')){
    if (is.na(max_ramp_rate)) {
      category <- 'Cannot be set'
    } else {
      if(max_ramp_rate <= compliance_threshold_ramp_rate){
        category <- 'Compliant'
      } else {
        category <- 'Too fast'
      }
    }
  } else {
    category <- "NA"
  }
  return(category)
}

categorise_reconnection_compliance_max_ramp <- function(reconnection_times, compliance_threshold_ramp_rate){
  reconnection_times <- rowwise(reconnection_times) %>% mutate(
    reconnection_compliance_status = catergorise_row_max_ramp(response_category, max_reconnection_ramp_rate,
                                                              compliance_threshold_ramp_rate))
  reconnection_times <- select(reconnection_times, c_id, reconnection_compliance_status)
  return(reconnection_times)
}

