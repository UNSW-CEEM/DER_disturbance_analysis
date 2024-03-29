#' Calculate ideal f-W response
#'
#' Once the frequency has been within f_hyst of f_ulco for t_hyst then the system should end f-W
#' and ramp back up
#'
#' @param f_ulco upper limit of continuous operation (the point at which f-W droop should start)
#' @param f_hyst hysteresis frequency value
#' @param t_hyst hysteresis time in seconds
#' @param f_upper frequency droop response upper limit. Default 52Hz in most cases
ideal_response <- function(frequency_data, f_ulco, f_hyst, t_hyst, f_upper) {
  frequency_data <- frequency_data[order(frequency_data$ts),]
  start_times <- c()
  end_times <- c()
  ts <- c()
  f <- c()
  norm_power <- c()
  for (i in 1:length(frequency_data$ts)) {
    # Look at last t_hyst seconds of data (the 2015 std default is 60s, the 2020 std default is 20s)
    last_60_seconds_of_data <- filter(
      frequency_data,
      (ts > frequency_data$ts[i] - t_hyst) & (ts <= frequency_data$ts[i])
    )
    # If there is a new over-frequency event (i.e. frequency goes above 50.25Hz and we're not already in a f-W droop)
    # then record this as a new start time.
    if (frequency_data$f[i] >= f_ulco & length(start_times) == length(end_times)) {
      start_times <- c(start_times, frequency_data$ts[i])
      # And if we don't have any data yet in the ts, then it's the first event, so record this ts. Else append this ts.
      if (length(ts) == 0) {
        ts <- c(frequency_data$ts[i])
      } else {
        ts <- c(ts, frequency_data$ts[i])
      }
      f <- c(f, frequency_data$f[i])
      # Ideal response profile at this time (when frequency is > 50.25Hz for the first time) is set to 1
      norm_power <- c(norm_power, 1)
    # Check for whether the frequency has been below 50.15Hz for at least 60s for the first time since the droop start
    } else if (max(last_60_seconds_of_data$f) <= (f_ulco - f_hyst) & length(start_times) > length(end_times)) {
      # If so, then add this time to end_times, record the ts, set norm_power to the last value in the norm power list
      end_times <- c(end_times, frequency_data$ts[i])
      ts <- c(ts, frequency_data$ts[i])
      norm_power <- c(norm_power, tail(norm_power, n = 1))
      f <- c(f, frequency_data$f[i])
    # If this interval is during a f-W droop window (may be the first interval!), then copy across these ts and
    # frequency data points
    } else if (length(start_times)>length(end_times)) {
      ts <- c(ts, frequency_data$ts[i])
      f <- c(f, frequency_data$f[i])
      # Check if frequency has reached a new maximum above 50.25Hz, else norm power is based on the previous norm power
      if (frequency_data$f[i] >= f_ulco) {
        norm_power <- c(
          norm_power,
          min(norm_p_over_frequency(frequency_data$f[i], f_ulco, f_upper), tail(norm_power, n = 1))
        )
      } else {
        norm_power <- c(norm_power, tail(norm_power, n = 1))
      }
    }
  }
  response_data <- data.frame(ts, f, norm_power, stringsAsFactors = FALSE)
  return(response_data)
}

norm_p_over_frequency <- function(f, f_ulco, f_upper) {
  if (f >= f_ulco && f <= f_upper) {
    norm_p <- 1 - ((f - f_ulco) / (f_upper - f_ulco))
  } else if (f > f_upper) {
    norm_p <- 0.0
  } else {
    norm_p = 1.0
  }
  return((norm_p))
}

down_sample_1s <- function(ideal_response_1_s, duration, offset) {
  if (duration != 1) {   # down-sampling is ignored for 1s data
    ideal_response_1_s <- thicken(
      ideal_response_1_s,
      paste(duration, "s"),
      colname = "time_group",
      rounding = "up",
      start_val = offset - as.numeric(duration)
    )
    ideal_response_1_s <- thicken(
      ideal_response_1_s,
      paste(duration, "s"),
      colname = "time_group2",
      rounding = "down",
      by = "ts",
      start_val = offset - as.numeric(duration)
    )
  ideal_response_1_s[ideal_response_1_s$ts == ideal_response_1_s$time_group2,]$time_group <-
    ideal_response_1_s[ideal_response_1_s$ts == ideal_response_1_s$time_group2,]$time_group2
  ideal_response_1_s <- ideal_response_1_s %>%
    filter(ts <= max(ideal_response_1_s[ideal_response_1_s$ts == ideal_response_1_s$time_group2,]$time_group2)) %>%
    filter(ts >= min(ideal_response_1_s[ideal_response_1_s$ts == (ideal_response_1_s$time_group2 + 1),]$ts))
  ideal_response_downsampled <- ideal_response_1_s %>%
    group_by(time_group) %>%
    summarise(f = last(f), norm_power = mean(norm_power)) %>%
    as.data.frame()
  
  } else if (duration == 1) {
      ideal_response_downsampled <- ideal_response_1_s
      colnames(ideal_response_downsampled)[1] <- "time_group"  # only column name is changed from the ideal response (for 1s data analysis)
  }
    
  return(ideal_response_downsampled)
}

calc_error_metric_and_compliance <- function(combined_data, ideal_response_downsampled) {
  error_by_c_id <- calc_error_metric(combined_data, ideal_response_downsampled)
  threshold_error <- calc_threshold_error(ideal_response_downsampled)
  error_by_c_id <- calc_compliance_status(error_by_c_id, threshold_error)
  combined_data <- left_join(combined_data, error_by_c_id, by = c("site_id"))
  return(combined_data)
}

calc_error_metric <- function(combined_data, ideal_response_downsampled) {
  combined_data <- combined_data %>%
    distinct(site_id, ts, .keep_all = TRUE) %>%
    inner_join(ideal_response_downsampled, by = c("ts" = "time_group")) %>%
    mutate(abs_percent_diff_actual_cf_ideal = abs(Site_Event_Normalised_Power_kW - norm_power) / norm_power) %>%
    mutate(percent_diff_actual_cf_ideal = (Site_Event_Normalised_Power_kW - norm_power) / norm_power)
  error_by_c_id <- combined_data %>%
    group_by(site_id) %>%
    summarise(
      min_diff = min(percent_diff_actual_cf_ideal),
      max_diff = max(percent_diff_actual_cf_ideal),
      abs_percent_diff_actual_cf_ideal = mean(abs_percent_diff_actual_cf_ideal),
      percent_diff_actual_cf_ideal = mean(percent_diff_actual_cf_ideal)
    ) %>%
    as.data.frame() %>%
    mutate(mixed_wrt_spec = 1) %>%
    mutate(below_spec = ifelse(max_diff <= 0, 1, 0)) %>%
    mutate(above_spec = ifelse(min_diff >= 0, 1, 0)) %>%
    mutate(mixed_wrt_spec = mixed_wrt_spec - below_spec - above_spec) %>%
    mutate(
      combined_error_metric = (
        (below_spec + above_spec) * percent_diff_actual_cf_ideal + mixed_wrt_spec * abs_percent_diff_actual_cf_ideal
      )
    )
  return(error_by_c_id)
}

calc_compliance_status <- function(error_by_c_id, threshold_error) {
  error_by_c_id <- error_by_c_id %>%
    mutate(compliance_status = ifelse(below_spec == 1, "Compliant", "Undefined")) %>%
    mutate(compliance_status = ifelse(above_spec == 1, "Above Ideal Response", compliance_status)) %>%
    mutate(
      compliance_status = ifelse(
        above_spec == 1 & combined_error_metric > threshold_error,
        "Non Compliant",
        compliance_status
      )
    ) %>%
    mutate(compliance_status = ifelse(mixed_wrt_spec == 1, "Ambigous", compliance_status))
  return(error_by_c_id)
}

calc_threshold_error <- function(ideal_response_downsampled) {
  threshold_error <- mean((1 - ideal_response_downsampled$norm_power) / ideal_response_downsampled$norm_power)
  return(threshold_error)
}

calc_error_metric_and_compliance_2 <- function(combined_data,
                                               ideal_response_downsampled,
                                               ideal_response,
                                               threshold,
                                               start_buffer,
                                               end_buffer,
                                               end_buffer_responding,
                                               disconnecting_threshold) {
  start_buffer_t <- min(ideal_response$ts) + start_buffer
  end_buffer <- max(ideal_response$ts) - end_buffer
  end_buffer_responding <- min(ideal_response$ts) + end_buffer_responding
  disconnecting_threshold <- disconnecting_threshold
  # If checking both 2015 and 2020 compliance status, then we need to store the first compliance assessment that's
  # already happened under a different col name before embarking on the next assessment.
  if ("compliance_status" %in% colnames(combined_data)) {
    combined_data <- dplyr::rename(combined_data, "compliance_status_2015" = "compliance_status")
  }
  # First pass compliance
  ideal_response_downsampled_f <- filter(ideal_response_downsampled, time_group >= start_buffer_t)
  ideal_response_downsampled_f <- filter(ideal_response_downsampled_f, time_group <= end_buffer)
  error_by_c_id <- combined_data %>%
    inner_join(ideal_response_downsampled_f, by = c("ts" = "time_group")) %>%
    mutate(error = (1 - c_id_norm_power) - ((1 - norm_power) * threshold)) %>%
    group_by(c_id, clean) %>%
    summarise(min_error = min(error)) %>%
    mutate(compliance_status = ifelse(min_error >= 0.0, "Compliant", "Non-compliant")) %>%
    select(c_id, compliance_status, clean)
  combined_data <- left_join(combined_data, error_by_c_id, by = c("c_id", "clean"))

  # Change 'Non compliant' to 'Non Compliant Responding' where compliant at start
  ideal_response_downsampled_f <- filter(ideal_response_downsampled, time_group <= end_buffer_responding)
  error_by_c_id <- combined_data %>%
    inner_join(ideal_response_downsampled_f, by = c("ts" = "time_group")) %>%
    mutate(error = (1 - c_id_norm_power) - ((1 - norm_power) * threshold)) %>%
    group_by(c_id, clean) %>%
    summarise(max_error = max(error), compliance_status = first(compliance_status)) %>%
    mutate(
      compliance_status = ifelse(
        (max_error >= 0.0) && (compliance_status == "Non-compliant"),
        "Non-compliant Responding",
        compliance_status
      )
    ) %>%
    select(c_id, compliance_status, clean)
  combined_data_without_compliance_status <- subset(combined_data, select = -c(compliance_status))
  combined_data <- left_join(combined_data_without_compliance_status, error_by_c_id, by = c("c_id", "clean"))

  # Set disconnecting categories
  # First check that the ideal response doesn't drop low (to close to zero), since many sites would therefore remain as
  # 'compliant' rather than 'Disconnect' etc.
  min_ideal_response <- min(ideal_response_downsampled$norm_power)
  if (min_ideal_response > disconnecting_threshold) {
    combined_data <- combined_data %>%
      mutate(
        compliance_status = ifelse(response_category == "4 Disconnect", "Disconnect/Drop to Zero", compliance_status)
      ) %>%
      mutate(
        compliance_status = ifelse(response_category == "3 Drop to Zero", "Disconnect/Drop to Zero", compliance_status)
      )
  }

  combined_data <- combined_data %>%
    mutate(compliance_status = ifelse(response_category == "7 UFLS Dropout", "UFLS Dropout", compliance_status)) %>%
    mutate(compliance_status = ifelse(response_category == "5 Off at t0", "Off at t0", compliance_status)) %>%
    mutate(compliance_status = ifelse(response_category == "6 Not enough data", "Not enough data", compliance_status))

  # Now check for whether there was already a compliance_status col when the function was called, rename the new one to
  # 2020 std, and keep both in the output combined_data.
  if ("compliance_status_2015" %in% colnames(combined_data)) {
    combined_data <- dplyr::rename(combined_data, "compliance_status_2020" = "compliance_status")
    combined_data <- dplyr::rename(combined_data, "compliance_status" = "compliance_status_2015")
  }

  return(combined_data)
}
