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
  
  # fill NAs with 0
  lapply(c('SYNC_a005_vfCheckUnderVoltage', 'SYNC_a010_vfCheckFreqWobble'), function(x) {
    combined_data[, paste0(x) := ifelse(is.na(combined_data[[x]]), 0, combined_data[[x]])]
  })

  
  return(combined_data)
}

assess_islands <- function(combined_data, event_time, window_length){
  
  event_window_data <- filter(combined_data, ts >= event_time & ts <= event_time + 60 * window_length)
  event_window_data <- filter(event_window_data, Islanded & response_category %in% c('3 Drop to Zero', '4 Disconnect'))
  
  event_window_data <- groupby(event_window_data, c_id)
  event_window_data <- summarise(event_window_data, 
                                 max_f=max(frequency), min_f=min(frequency), 
                                 max_v=(max(voltage) - 240) / 240, min_v=(240 - min(voltage)) / 240)
  
  event_window_data <- mutate(event_window_data, island_assessment=ifelse(max_f > 53, 'Gateway curtailed', 'Undefined'))
  event_window_data <- mutate(event_window_data, 
                              island_assessment=ifelse(island_assessment=='Undefined' & (min_f < 49.8 | max_f > 50.2), 
                                                       'Frequency disruption', island_assessment))
  event_window_data <- mutate(event_window_data, 
                              island_assessment=ifelse(island_assessment=='Undefined' & (max_v > 0.1 | min_v > 0.1), 
                                                       'Voltage disruption', island_assessment))
  event_window_data <- mutate(event_window_data, 
                              island_assessment=ifelse(island_assessment=='Undefined',
                                                       'PV disconnect', island_assessment))

  return(event_window_data)
  
}