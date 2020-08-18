
catergorise_row <- function(response_category, reconnection_time, compliance_threshold_minutes){
  if (response_category %in% c('4 Disconnect', '3 Drop to Zero')){
    if (is.na(reconnection_time)) {
      category <- 'Cannot be set'
    } else {
      if(reconnection_time >= compliance_threshold_minutes){
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

categorise_reconnection_compliance <- function(reconnection_times, compliance_threshold_minutes){
  reconnection_times <- rowwise(reconnection_times) %>% mutate(
    reconnection_compliance_status = catergorise_row(response_category, reconnection_time,
                                                                               compliance_threshold_minutes))
  reconnection_times <- select(reconnection_times, c_id, reconnection_compliance_status)
  return(reconnection_times)
}

