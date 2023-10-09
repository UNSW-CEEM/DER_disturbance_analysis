create_reconnection_profile <- function(pre_event_time, ramp_length_minutes, time_step_seconds) {
  ramp_length_seconds <- ramp_length_minutes * 60
  reconnect_time <- pre_event_time + time_step_seconds + 60
  ramp_time_series <- seq(from = reconnect_time, to = reconnect_time + ramp_length_seconds, by = 1)
  ramp_time_series <- data.frame("ts" = ramp_time_series)
  ramp_time_series <- dplyr::mutate(
    ramp_time_series,
    norm_power = as.numeric((ts - reconnect_time) / ramp_length_seconds)
  )
  return(ramp_time_series)
}
