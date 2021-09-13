
ufls_detection_voltage <- function(combined_data, event_time, window_length){
  pre_event_window_start <- event_time - 60 * window_length
  post_event_window_end <- event_time + 60 * window_length
  pre_event_window <- filter(combined_data, ts > pre_event_window_start, ts < event_time)
  post_event_window <- filter(combined_data, ts > event_time, ts < post_event_window_end)

  pre_event_voltage <- calc_average_voltage_per_circuit(pre_event_window)
  names(pre_event_voltage)[names(pre_event_voltage) == 'v_mean'] <- 'pre_event_v_mean'

  post_event_voltage <- calc_average_voltage_per_circuit(post_event_window)
  names(post_event_voltage)[names(post_event_voltage) == 'v_mean'] <- 'post_event_v_mean'

  voltage_by_c_id <- merge(pre_event_voltage, post_event_voltage,
                           by=c('c_id', 'c_id'), all = TRUE)
  voltage_by_c_id <- mutate(voltage_by_c_id, pre_event_v_mean = ifelse(is.na(pre_event_v_mean),
                                                                      0, pre_event_v_mean))
  voltage_by_c_id <- mutate(voltage_by_c_id, post_event_v_mean = ifelse(is.na(post_event_v_mean),
                                                                       0, post_event_v_mean))
  ufls_dropout_voltage <- mutate(voltage_by_c_id,
                         ufls_status_v = if_else((post_event_v_mean/pre_event_v_mean) < 0.2,
                                               'UFLS Dropout', 'No UFLS Dropout'))
  return(ufls_dropout_voltage)
}

calc_average_voltage_per_circuit <- function(ts_data){
  ts_data <- group_by(ts_data, c_id)
  ts_data <- mutate(ts_data, v = as.numeric(v))
  ts_data <- data.frame(summarise(ts_data, v_mean = mean(v)))
  return(ts_data)
}
