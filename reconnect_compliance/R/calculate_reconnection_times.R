
calculate_reconnection_times <- function(normalised_power_profiles, event_time, disconnect_threshold, 
                                         reconnect_threshold){
  normalised_power_profiles <- filter(normalised_power_profiles, ts > event_time)
  
  distconnected_intervals <- filter(normalised_power_profiles, c_id_norm_power < disconnect_threshold)
  last_disconnected_interval <- group_by(distconnected_intervals, c_id)
  last_disconnected_interval <- summarise(last_disconnected_interval, pre_reconnection_time = max(ts))
  
  connected_intervals <- filter(normalised_power_profiles, c_id_norm_power > reconnect_threshold)
  first_fully_connected_interval <- group_by(connected_intervals, c_id)
  first_fully_connected_interval <- summarise(first_fully_connected_interval, fully_connected_time = min(ts))
  
  reconnection_times <- inner_join(last_disconnected_interval, first_fully_connected_interval, by='c_id')
  reconnection_times <- mutate(reconnection_times, reconnection_time = fully_connected_time - pre_reconnection_time)
  reconnection_times <- select(reconnection_times, c_id, reconnection_time)
  reconnection_times <- filter(reconnection_times, reconnection_time > 0.0)
}