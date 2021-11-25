# TODO: add col to circ_sum with islanded flags, also for freqwobble and undervoltage. 
# TODO: determine which islanded sites can be classified as disconnect
# TODO: replace response_category with island_assessment response
# TODO: determine and apply hierarchy of responses
# TODO: figure out which other functions rely on the responses/ need islanded sites removed

classify_islands <- function(combined_data, alert_data, event_time){
  if (~all(is.na(alert_data[,first_timestamp]))){
    alert_data[, first_timestamp := as.POSIXct((first_timestamp)/1000, tz="Australia/Brisbane", origin="1970-01-01")]
    alert_data[, Islanded := ifelse(is.na(first_timestamp),0,
                                    ifelse((GridFaultContactorTrip >= 1 | SYNC_a038_DoOpenArguments >= 1)
                                           & (first_timestamp < (event_time+30)),
                                           1,0))]
  } else {
    alert_data[, Islanded := ifelse((GridFaultContactorTrip >= 1 | SYNC_a038_DoOpenArguments >= 1), 1,0)]
  }

  combined_data <- merge(combined_data, alert_data[,c('c_id', 
                                                  'Islanded',
                                                  'SYNC_a005_vfCheckUnderVoltage', 
                                                  'SYNC_a010_vfCheckFreqWobble')],
                         by = c('c_id'), all.x = TRUE)
  
  # remove NAs
  lapply(colnames(combined_data)[5:ncol(combined_data)], function(x) {
    combined_data[, paste0(x) := ifelse(is.na(combined_data[[x]]), 0, combined_data[[x]])]
  })

  
  return(combined_data)
}