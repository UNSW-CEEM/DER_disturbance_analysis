event_normalised_power <- function(combined_data, event_time, keep_site_id) {
  event_time_data <- data.frame(filter(combined_data, Time > event_time - d & Time <= event_time))
  event_time_data <- setnames(event_time_data, c("site_performance_factor"), c("event_site_performance_factor"))
  if (keep_site_id) {
    event_time_data <- select(event_time_data, site_id, event_site_performance_factor)
    event_time_data <- distinct(event_time_data)
    combined_data <- left_join(combined_data, event_time_data, by = "site_id")
  } else {
    event_time_data <- select(event_time_data, series, event_site_performance_factor)
    combined_data <- left_join(combined_data, event_time_data, by = "series")
  }
  combined_data <- mutate(
    combined_data,
    Event_Normalised_Power_kW = ifelse(
      event_site_performance_factor > 0.00001,
      site_performance_factor / event_site_performance_factor,
      NA
    )
  )
  return(combined_data)
}
