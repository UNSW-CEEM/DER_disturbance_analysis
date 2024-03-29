create_reconnection_summary <- function(combined_data_f,
                                        duration,
                                        pre_event_interval,
                                        disconnecting_threshold,
                                        reconnect_threshold,
                                        ramp_rate_threshold,
                                        ramp_threshold_for_compliance,
                                        ramp_threshold_for_non_compliance,
                                        ramp_rate_change_resource_limit_threshold) {
  post_event_response <- select(combined_data_f, ts, c_id, c_id_daily_norm_power, pre_event_norm_power, d)
  post_event_response <- filter(post_event_response, ts > pre_event_interval)

  post_event_response <- post_event_response[order(post_event_response$c_id,post_event_response$ts),]
  
  # downsample to 5s only during 1s data analysis
    if (duration == 1) {
      post_event_response_5s_ds <- downsample_for_reconnection(post_event_response)
      post_event_response_5s_ds$c_id_daily_norm_power[is.nan(post_event_response_5s_ds$c_id_daily_norm_power)] <- NA
      post_event_response <- post_event_response_5s_ds
      post_event_response$d <- 5
    } else {
      post_event_response <- post_event_response
    }
  
  reconnection_times <- calculate_reconnection_times(
    post_event_response,
    event_time = pre_event_interval,
    disconnect_threshold = disconnecting_threshold,
    reconnect_threshold = reconnect_threshold
  )

  ramp_rates <- calculate_ramp_rates(post_event_response)
  reconnection_start_times <- find_last_disconnected_intervals(
    post_event_response,
    disconnect_threshold = disconnecting_threshold
  )
  ramp_after_connection_begins <- inner_join(ramp_rates, reconnection_start_times, by = c("c_id"))
  ramp_after_connection_begins <- filter(ramp_after_connection_begins, ts >= pre_reconnection_time)

  resource_limited_intervals <- find_first_resource_limited_interval(
    ramp_after_connection_begins,
    ramp_rate_change_threshold = ramp_rate_change_resource_limit_threshold
  )

  reconnection_data <- inner_join(post_event_response, ramp_rates, by = c("c_id", "ts"))
  reconnection_data <- left_join(reconnection_data, resource_limited_intervals, by = c("c_id"))

  max_ramp_rates <- calculate_total_ramp_while_exceeding_ramp_rate_compliance_threshold(
    reconnection_data,
    event_time = pre_event_interval,
    disconnect_threshold = disconnecting_threshold,
    reconnect_threshold = reconnect_threshold,
    ramp_rate_threshold = ramp_rate_threshold
  )

  reconnection_summary <- combined_data_f %>%
    group_by(c_id) %>%
    summarise(
      response_category = first(response_category),
      pre_event_daily_norm_power = first(pre_event_norm_power)
    ) %>%
    left_join(reconnection_times, by = c("c_id")) %>%
    left_join(max_ramp_rates, by = c("c_id"))

  if (dim(reconnection_summary)[1] == 0) {
    reconnection_categories <- data.frame(matrix(ncol = 2, nrow = 0))
    names <- c("c_id", "reconnection_compliance_status")
    colnames(reconnection_categories) <- names
  } else {
    reconnection_categories <- categorise_reconnection_compliance(
      reconnection_summary,
      ramp_threshold_for_compliance,
      ramp_threshold_for_non_compliance
    )
  }

  reconnection_categories <- reconnection_categories %>%
    inner_join(
      select(reconnection_summary, c_id, reconnection_time, ramp_above_threshold, pre_event_daily_norm_power),
      by = c("c_id")
    ) %>%
    as.data.frame()

  return(reconnection_categories)
}
