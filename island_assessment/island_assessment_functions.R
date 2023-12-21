classify_islands <- function(combined_data, alert_data, event_time, window_length) {
  # add col to circ_sum with islanded flags, also for freqwobble and undervoltage.
  combined_data <- identify_islanded_sites(combined_data, alert_data, event_time)
  # determine which islanded sites can be classified as disconnect
  event_window_data <- filter(combined_data, ts > event_time - d & ts <= event_time + 60 * window_length)
  event_window_data <- assess_islands(event_window_data)
  combined_data <- left_join(combined_data, event_window_data, by = c("c_id", "clean"))
  # replace response_category with island_assessment response
  combined_data <- replace_response_with_alert(combined_data)
  return(combined_data)
}

identify_islanded_sites <- function(combined_data, alert_data, event_time) {
  if (!all(is.na(alert_data$first_timestamp))) {
    alert_data <- alert_data %>%
      mutate(first_timestamp = as.POSIXct(first_timestamp / 1000, tz = "Australia/Brisbane", origin = "1970-01-01")) %>%
      mutate(
        Islanded = ifelse(
          is.na(first_timestamp),
          0,
          ifelse(
            (
              (GridFaultContactorTrip >= 1 | SYNC_a038_DoOpenArguments >= 1) &
              ((first_timestamp < (event_time + 60)) & (first_timestamp >= event_time))
            ),
            1,
            0
          )
        )
      )
  } else {
    alert_data <- mutate(
      alert_data,
      Islanded = ifelse(GridFaultContactorTrip >= 1 | SYNC_a038_DoOpenArguments >= 1, 1, 0)
    )
  }

  combined_data <- left_join(
    combined_data,
    alert_data[, c("c_id", "Islanded", "SYNC_a005_vfCheckUnderVoltage", "SYNC_a010_vfCheckFreqWobble")],
    by = c("c_id")
  )
  setnafill(combined_data, cols = c("Islanded"), fill = 0, type = "const")

  combined_data[c("SYNC_a005_vfCheckUnderVoltage", "SYNC_a010_vfCheckFreqWobble")][
    is.na(combined_data[c("SYNC_a005_vfCheckUnderVoltage", "SYNC_a010_vfCheckFreqWobble")])
  ] <- 0

  return(combined_data)
}

assess_islands <- function(event_window_data) {
  event_window_data <- filter(event_window_data, Islanded == 1)
  if (length(event_window_data$c_id) > 0) {
    event_window_data <- event_window_data %>%
      mutate(voltage = as.numeric(v), frequency = as.numeric(f)) %>%
      mutate(
        vmin = if (all(is.na(event_window_data$vmin))) v else vmin,
        vmax = if (all(is.na(event_window_data$vmax))) v else vmax,
        fmin = if (all(is.na(event_window_data$fmin))) f else fmin,
        fmax = if (all(is.na(event_window_data$fmax))) f else fmax
      ) %>%
      group_by(c_id, clean) %>%
      summarise(
        response_category = first(response_category),
        max_f = max(fmax,na.rm=TRUE),
        min_f = min(fmin,na.rm=TRUE),
        max_v = (max(vmax,na.rm=TRUE) - 240) / 240,
        min_v = (240 - min(vmin,na.rm=TRUE)) / 240
      ) %>%
      mutate(island_assessment = ifelse(max_f > 53, "Gateway curtailed", "Undefined")) %>%
      mutate(
        island_assessment = ifelse(
          island_assessment == "Undefined" & (min_f < 49.8 | max_f > 50.2),
          "Frequency disruption",
          island_assessment
        )
      ) %>%
      mutate(
        island_assessment = ifelse(
          island_assessment == "Undefined" & (max_v > 0.1 | min_v > 0.1),
          "Voltage disruption",
          island_assessment
        )
      ) %>%
      mutate(
        island_assessment = ifelse(
          island_assessment == "Undefined" & response_category %in% c("3 Drop to Zero", "4 Disconnect"),
          "PV disconnect",
          island_assessment
        )
      ) %>%
      mutate(island_assessment = ifelse(island_assessment == "Undefined", "Otherwise normal", island_assessment))
  } else {
    event_window_data <- mutate(event_window_data, island_assessment = "NA")
  }
  event_window_data <- select(event_window_data, "c_id", "clean", "island_assessment")
  return(event_window_data)
}

replace_response_with_alert <- function(combined_data) {
  combined_data <- combined_data %>%
    mutate(islanding_alert = ifelse(Islanded, "Islanded - Other", "NA")) %>%
    mutate(
      islanding_alert = ifelse(
        Islanded & SYNC_a005_vfCheckUnderVoltage >= 1,
        "Islanded - Under Volt alert",
        islanding_alert
      )
    ) %>%
    mutate(
      islanding_alert = ifelse(Islanded & SYNC_a010_vfCheckFreqWobble >= 1, "Islanded - Freq Wobble", islanding_alert)
    )
  combined_data <- combined_data[,
    !(names(combined_data) %in% c("SYNC_a005_vfCheckUnderVoltage", "SYNC_a010_vfCheckFreqWobble"))
  ]
  return(combined_data)
}
