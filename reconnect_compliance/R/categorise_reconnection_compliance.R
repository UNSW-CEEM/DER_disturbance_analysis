
catergorise_row <- function(response_category, 
                            reconnection_time,
                            max_reconnection_ramp_rate,
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
    }
  } else {
    category <- "NA"
  }
  return(category)
}

categorise_reconnection_compliance <- function(reconnection_times,
                                               reconnection_time_threshold_for_compliance,
                                               reconnection_time_threshold_for_non_compliance,
                                               ramp_rate_threshold_for_compliance,
                                               ramp_rate_threshold_for_non_compliance){
  
  reconnection_times <- rowwise(reconnection_times) %>% mutate(
    reconnection_compliance_status = catergorise_row(response_category, 
                                                     reconnection_time,
                                                     max_reconnection_ramp_rate,
                                                     reconnection_time_threshold_for_compliance,
                                                     reconnection_time_threshold_for_non_compliance,
                                                     ramp_rate_threshold_for_compliance,
                                                     ramp_rate_threshold_for_non_compliance))
  
  reconnection_times <- select(reconnection_times, c_id, reconnection_compliance_status)
  return(reconnection_times)
}

