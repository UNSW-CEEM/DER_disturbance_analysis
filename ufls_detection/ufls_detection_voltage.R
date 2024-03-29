ufls_detection_voltage <- function(combined_data,
                                   event_time,
                                   window_length,
                                   fill_nans = FALSE,
                                   post_event_delay = 0) {
  pre_event_window_start <- event_time - 60 * window_length
  post_event_window_end <- event_time + 60 * window_length
  pre_event_window <- filter(combined_data, ts > pre_event_window_start, ts <= event_time)
  post_event_window <- filter(
    combined_data,
    ts > event_time + post_event_delay,
    ts <= post_event_window_end + post_event_delay
  )

  pre_event_voltage <- calc_average_voltage_per_circuit(pre_event_window)
  names(pre_event_voltage)[names(pre_event_voltage) == "v_mean"] <- "pre_event_v_mean"

  post_event_voltage <- calc_average_voltage_per_circuit(post_event_window)
  names(post_event_voltage)[names(post_event_voltage) == "v_mean"] <- "post_event_v_mean"

  voltage_by_c_id <- merge(pre_event_voltage, post_event_voltage, by = "c_id", all = TRUE)

  if (fill_nans) {
    setnafill(voltage_by_c_id, cols = c("post_event_v_mean"), fill = 0, type = "const")
  }

  ufls_dropout_voltage <- mutate(
    voltage_by_c_id,
    ufls_status_v = if_else(
      (pre_event_v_mean > 180) & (post_event_v_mean < 180),
      "UFLS Dropout",
      if_else(pre_event_v_mean > 180, "No UFLS Dropout", NA_character_)
    )
  )
  return(ufls_dropout_voltage)
}

calc_average_voltage_per_circuit <- function(ts_data) {
  ts_data <- group_by(ts_data, c_id)
  ts_data <- mutate(ts_data, v = as.numeric(v))
  ts_data <- data.frame(summarise(ts_data, v_mean = mean(v, na.rm = TRUE)))
  return(ts_data)
}
