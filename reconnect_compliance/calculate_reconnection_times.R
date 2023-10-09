calculate_reconnection_times <- function(normalised_power_profiles,
                                         event_time,
                                         disconnect_threshold,
                                         reconnect_threshold) {
  normalised_power_profiles <- filter(normalised_power_profiles, ts > event_time)

  last_disconnected_interval <- find_last_distconnected_intervals(normalised_power_profiles, disconnect_threshold)

  connected_intervals <- filter(
    normalised_power_profiles,
    c_id_daily_norm_power > pre_event_norm_power * reconnect_threshold
  )
  connected_intervals <- inner_join(connected_intervals, last_disconnected_interval, by = "c_id")
  connected_intervals <- filter(connected_intervals, ts > pre_reconnection_time)
  first_fully_connected_interval <- group_by(connected_intervals, c_id)
  first_fully_connected_interval <- summarise(first_fully_connected_interval, fully_connected_time = min(ts))

  reconnection_times <- inner_join(last_disconnected_interval, first_fully_connected_interval, by = "c_id")
  reconnection_times <- mutate(
    reconnection_times,
    reconnection_time = as.numeric(difftime(fully_connected_time, pre_reconnection_time, units = "mins"))
  )
  reconnection_times <- select(reconnection_times, c_id, reconnection_time)
  reconnection_times <- filter(reconnection_times, reconnection_time > 0.0)
  return(reconnection_times)
}

find_last_distconnected_intervals <- function(normalised_power_profiles, disconnect_threshold) {
  distconnected_intervals <- filter(
    normalised_power_profiles,
    c_id_daily_norm_power < pre_event_norm_power * disconnect_threshold
  )
  last_disconnected_interval <- group_by(distconnected_intervals, c_id)
  last_disconnected_interval <- summarise(last_disconnected_interval, pre_reconnection_time = max(ts))
  return(last_disconnected_interval)
}
