create_reconnection_summary <- function(combined_data_f, pre_event_interval,
                                        disconnecting_threshold,
                                        max_norm_output_threshold, 
                                        reconnection_time_threshold_for_compliance,
                                        reconnection_time_threshold_for_non_compliance,
                                        ramp_rate_threshold_for_compliance,
                                        ramp_rate_threshold_for_non_compliance){
  max_norm_power <- group_by(combined_data_f, c_id) %>% summarise(max_norm_power = max(c_id_norm_power))
  
  post_event_response <- select(combined_data_f, ts, c_id, c_id_norm_power)
  post_event_response <- filter(post_event_response, ts > pre_event_interval)
  
  reconnection_times <- calculate_reconnection_times(post_event_response, 
                                                     event_time = pre_event_interval, 
                                                     disconnect_threshold = disconnecting_threshold, 
                                                     reconnect_threshold = 0.95)
  combined_data_f <- inner_join(combined_data_f, reconnection_times, by = 'c_id')
  ramp_rates <- calculate_ramp_rates(combined_data_f)
  reconnection_start_times <- find_last_distconnected_intervals(post_event_response, 
                                                                disconnect_threshold = disconnecting_threshold)
  ramp_after_connection_begins <- inner_join(ramp_rates, reconnection_start_times, by = 'c_id')
  ramp_after_connection_begins <- filter(ramp_after_connection_begins, ts >= pre_reconnection_time)
  resource_limited_intervals <- find_first_resource_limited_interval(ramp_after_connection_begins, 
                                                                     ramp_rate_change_threshold = -0.10)
  reconnection_data <- inner_join(post_event_response, ramp_rates, by = c('c_id', 'ts'))
  reconnection_data <- left_join(reconnection_data, resource_limited_intervals, by = 'c_id')
  max_ramp_rates <- calculate_max_reconnection_ramp_rate(reconnection_data, event_time = pre_event_interval, 
                                                         disconnect_threshold = disconnecting_threshold, 
                                                         reconnect_threshold = 0.95)
  reconnection_summary <- group_by(combined_data_f, c_id)
  reconnection_summary <- summarise(reconnection_summary, response_category = first(response_category),
                                    max_norm_power = max(c_id_norm_power))
  reconnection_summary <- inner_join(reconnection_summary, reconnection_times, by = 'c_id')
  reconnection_summary <- inner_join(reconnection_summary, max_ramp_rates, by = 'c_id')
  reconnection_categories <- categorise_reconnection_compliance(reconnection_summary,  
                                                                max_norm_output_threshold, 
                                                                reconnection_time_threshold_for_compliance,
                                                                reconnection_time_threshold_for_non_compliance,
                                                                ramp_rate_threshold_for_compliance,
                                                                ramp_rate_threshold_for_non_compliance)
  reconnection_categories <- inner_join(reconnection_categories, select(reconnection_summary, c_id, reconnection_time,
                                                                        max_reconnection_ramp_rate, max_norm_power), 
                                        by = 'c_id')
  return(reconnection_categories)
}