
calculate_max_reconnection_ramp_rate <- function(normalised_power_profiles, event_time, disconnect_threshold, 
                                         reconnect_threshold){
  normalised_power_profiles <- filter(normalised_power_profiles, ts > event_time)
  
  distconnected_intervals <- filter(normalised_power_profiles, c_id_daily_norm_power < disconnect_threshold * pre_event_norm_power)
  last_disconnected_interval <- group_by(distconnected_intervals, c_id)
  last_disconnected_interval <- summarise(last_disconnected_interval, pre_reconnection_time = max(ts))
  
  connected_intervals <- filter(normalised_power_profiles, c_id_daily_norm_power > pre_event_norm_power * reconnect_threshold)
  connected_intervals <- inner_join(connected_intervals, last_disconnected_interval, by='c_id')
  connected_intervals <- filter(connected_intervals, ts > pre_reconnection_time)
  first_fully_connected_interval <- group_by(connected_intervals, c_id)
  first_fully_connected_interval <- summarise(first_fully_connected_interval, fully_connected_time = min(ts))
  
  reconnection_times <- left_join(last_disconnected_interval, first_fully_connected_interval, by = 'c_id')
  normalised_power_profiles <- inner_join(normalised_power_profiles, reconnection_times, by = 'c_id')
  normalised_power_profiles <- filter(normalised_power_profiles, ts >= pre_reconnection_time)
  normalised_power_profiles <- filter(normalised_power_profiles, ts <= fully_connected_time | is.na(fully_connected_time))
  
  normalised_power_profiles <- mutate(normalised_power_profiles, resource_limited_interval = 
                                                                 ifelse(is.na(resource_limited_interval), 
                                                                        fully_connected_time, 
                                                                        resource_limited_interval))
  normalised_power_profiles <- filter(normalised_power_profiles, ts <= resource_limited_interval | is.na(fully_connected_time))
  
  max_ramp_rates <- group_by(normalised_power_profiles, c_id)
  max_ramp_rates <- as.data.frame(summarise(max_ramp_rates, max_reconnection_ramp_rate = max(ramp_rate, na.rm = TRUE)))
}