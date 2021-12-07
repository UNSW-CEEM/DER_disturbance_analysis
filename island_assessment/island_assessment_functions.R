classify_islands <- function(combined_data, alert_data, event_time, window_length){
  # add col to circ_sum with islanded flags, also for freqwobble and undervoltage. 
  combined_data <- identify_islanded_sites(combined_data, alert_data, event_time)
  # determine which islanded sites can be classified as disconnect
  event_window_data <- filter(combined_data, ts >= event_time & ts <= event_time + 60 * window_length)
  event_window_data <- assess_islands(event_window_data)
  combined_data <- left_join(combined_data, event_window_data, on=c("c_id", "clean"))
  # replace response_category with island_assessment response
  combined_data <- replace_response_with_alert(combined_data)
  return(combined_data)
}

# TODO: determine and apply hierarchy of responses
# TODO: figure out which other functions rely on the responses/ need islanded sites removed

identify_islanded_sites <- function(combined_data, alert_data, event_time){
  if (!all(is.na(alert_data$first_timestamp))){
    alert_data <- mutate(alert_data, first_timestamp = as.POSIXct((first_timestamp)/1000, tz="Australia/Brisbane", 
                                                                  origin="1970-01-01"))
    alert_data <- mutate(alert_data, Islanded = ifelse(is.na(first_timestamp),0,
                                    ifelse((GridFaultContactorTrip >= 1 | SYNC_a038_DoOpenArguments >= 1)
                                           & (first_timestamp < (event_time+30)),
                                           1,0)))
  } else {
    alert_data <- mutate(alert_data, Islanded = ifelse((GridFaultContactorTrip >= 1 | SYNC_a038_DoOpenArguments >= 1), 1,0))
  }

  combined_data <- left_join(combined_data, alert_data[,c('c_id', 
                                                  'Islanded',
                                                  'SYNC_a005_vfCheckUnderVoltage', 
                                                  'SYNC_a010_vfCheckFreqWobble')],
                         by = c('c_id'))
  
  #TODO: missing values in undervolt and freqwobble cols need to be filled with 0
  
  #lapply(c('SYNC_a005_vfCheckUnderVoltage', 'SYNC_a010_vfCheckFreqWobble'), function(x) {
  #  combined_data[, paste0(x) := ifelse(is.na(combined_data[[x]]), 0, combined_data[[x]])]
  #})

  
  return(combined_data)
}

assess_islands <- function(event_window_data){
  event_window_data <- filter(event_window_data, Islanded & response_category %in% c('3 Drop to Zero', '4 Disconnect'))
  event_window_data <- group_by(event_window_data, c_id, clean)
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

  event_window_data <- select(event_window_data, "c_id", "clean", "island_assessment")
  return(event_window_data)
}

replace_response_with_alert <- function(combined_data){
  combined_data <- mutate(combined_data, response_category=ifelse(Islanded, 'Islanded - Other', response_category))
  combined_data <- mutate(combined_data, response_category=ifelse(SYNC_a005_vfCheckUnderVoltage>=1, 
                                                                  'Islanded - Under Volt alert', response_category))
  combined_data <- mutate(combined_data, response_category=ifelse(SYNC_a010_vfCheckFreqWobble>=1, 
                                                                  'Islanded - Freq Wobble', response_category))
  
  return(combined_data)
}