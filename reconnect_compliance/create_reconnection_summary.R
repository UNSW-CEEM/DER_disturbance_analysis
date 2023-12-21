create_reconnection_summary <- function(combined_data_f,
                                        pre_event_interval,
                                        disconnecting_threshold,
                                        reconnect_threshold,
                                        ramp_rate_threshold,
                                        ramp_threshold_for_compliance,
                                        ramp_threshold_for_non_compliance,
                                        ramp_rate_change_resource_limit_threshold) {
  post_event_response <- select(combined_data_f, ts, c_id, c_id_daily_norm_power, pre_event_norm_power)
  post_event_response <- filter(post_event_response, ts > pre_event_interval)

  post_event_response <- post_event_response[order(post_event_response$c_id,post_event_response$ts),]
  
  # downsample to 5s during 1s data analysis
  calculate_downsample <- function(post_event_response) {
    time_intervals <- seq(min(post_event_response$ts), max(post_event_response$ts), by = 5)
    avg_power <- sapply(time_intervals, function(interval) {
      mean(post_event_response$c_id_daily_norm_power[post_event_response$ts >= interval & post_event_response$ts < interval + 5], na.rm = TRUE)
    })
    
    post_event_response_5S <- data.frame(
      c_id = unique(post_event_response$c_id)[1],
      ts = time_intervals,
      c_id_daily_norm_power = avg_power,
      pre_event_norm_power = unique(post_event_response$pre_event_norm_power)[1]
    )
    return(post_event_response_5S)
  }
  
  post_event_response_5S_ds <- post_event_response %>%
    group_by(c_id) %>%
    do(calculate_downsample(.)) %>%
    ungroup()
  
  post_event_response_5S_ds$c_id_daily_norm_power[is.nan(post_event_response_5S_ds$c_id_daily_norm_power)] <- NA
  post_event_response <- post_event_response_5S_ds
  post_event_response$d<- 5
  # end of downsampling and processing data for 1s
  
  reconnection_times <- calculate_reconnection_times(
    post_event_response,
    event_time = pre_event_interval,
    disconnect_threshold = disconnecting_threshold,
    reconnect_threshold = reconnect_threshold
  )

  # ramp_rates <- calculate_ramp_rates(combined_data_f)
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
  # reconnection_data <- left_join(reconnection_data, select(combined_data_f, c_id, ts, d), by = c("c_id", "ts"))

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
