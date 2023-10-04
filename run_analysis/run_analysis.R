if (!exists("app_logger")) {
    logging::basicConfig()
    logger <- getLogger()$name
} else {
    logger <- app_logger
}

#' Check pre-event interval value is in data and matches time window
#' @param pre_event_interval The pre-event timestamp to be checked
#' @param load_start_time The start boundary timestamp for loaded data
#' @param load_start_time The end boundary timestamp for loaded data
#' @param window_length The input window length
#' @param data The dataframe containing the loaded data
validate_pre_event_interval <- function(pre_event_interval, load_start_time, load_end_time, window_length, data) {
  logdebug("Validating pre_event_interval", logger=logger)
  # check pre-event interval is on the selected time offset
  errors <- list(errors=list(), warnings=list())
  start_time <- as.POSIXct(load_start_time, tz = "Australia/Brisbane")
  end_time <- as.POSIXct(load_end_time, tz = "Australia/Brisbane")
  pre_event_interval <- as.POSIXct(pre_event_interval, tz = "Australia/Brisbane")
  data_at_set_pre_event_interval = filter(data, ts > pre_event_interval - d & ts <= pre_event_interval)
  if (dim(data_at_set_pre_event_interval)[1] == 0) {
    long_error_message <- c("The pre-event time interval does not match any time step in the time series data.")
    long_error_message <- paste(long_error_message, collapse = '')
    errors$errors[[length(errors$errors) + 1]] <- list(title="Error in pre-event time interval", body=long_error_message)
  }
  # check pre-event interval is inside the time window loaded
  if ((pre_event_interval < start_time) | (pre_event_interval > end_time)) {
    long_error_message <- c("The pre-event interval must be within the time window of loaded data.")
    long_error_message <- paste(long_error_message, collapse = '')
    errors$errors[[length(errors$errors) + 1]] <- list(title="Error in pre-event time interval", body=long_error_message)
  }
  # check window length does not extend outside time window loaded.
  if ((pre_event_interval + window_length * 60) > end_time) {
    long_error_message <- c("The event window length must be within the time window of loaded data.")
    long_error_message <- paste(long_error_message, collapse = '')
    errors$errors[[length(errors$errors) + 1]] <- list(title="Error in pre-event time interval", body=long_error_message)
  }
  return(errors)
}

#' Get the ideal frequency-watt response for a region
#' @param frequency_data The frequency data input to the tool
#' @param region_to_load Which region (state) to pull from the frequency data
#' @return Ideal response time series dataframe
ideal_response_from_frequency <- function(frequency_data, region_to_load, f_ulco, f_hyst, t_hyst, f_upper) {
  if (dim(frequency_data)[1] > 0) {
    temp_f_data <- select(frequency_data, ts, region_to_load)
    temp_f_data <- setnames(temp_f_data, c(region_to_load), c("f"))
    temp_f_data <- mutate(temp_f_data, f = as.numeric(f))
    ideal_response_to_plot <- ideal_response(temp_f_data, f_ulco, f_hyst, t_hyst, f_upper)
  } else {
    temp_f_data <- data.frame()
    ideal_response_to_plot <- data.frame()
  }
  results <- list()
  results$region_frequency <- temp_f_data
  results$ideal_response_to_plot <- ideal_response_to_plot
  return(results)
}

#' Apply user filters to combined data
filter_combined_data <- function(combined_data, off_grid_postcodes, cleaned, size_groupings, standards, postcodes,
                                manufacturers, models, sites, circuits) {
  logdebug("Apply user filters to combined data", logger=logger)
  combined_data_f <- combined_data
  site_types <- c("pv_site_net", "pv_site", "pv_inverter_net", "pv_inverter")
  if (length(cleaned) > 0) {combined_data_f <- filter(combined_data_f, clean %in% cleaned)}
  combined_data_f <- filter(combined_data_f, sum_ac<=100)
  if (length(site_types) > 0 ) {combined_data_f <- filter(combined_data_f, con_type %in% site_types)}
  if (length(off_grid_postcodes) > 0 ) {
    combined_data_f <- filter(combined_data_f, !(s_postcode %in% off_grid_postcodes))}
  if (length(size_groupings) > 0 ) {combined_data_f <- filter(combined_data_f, Grouping %in% size_groupings)}
  if (length(standards) > 0 ) {combined_data_f <- filter(combined_data_f, Standard_Version %in% standards)}
  if (length(postcodes) > 0) {combined_data_f <- filter(combined_data_f, s_postcode %in% postcodes)}
  if (length(manufacturers) > 0) {combined_data_f <- filter(combined_data_f, manufacturer %in% manufacturers)}
  if (length(models) > 0) {combined_data_f <- filter(combined_data_f, model %in% models)}
  if (length(sites) > 0) {combined_data_f <- filter(combined_data_f, site_id %in% sites)}
  if (length(circuits) > 0) {combined_data_f <- filter(combined_data_f, c_id %in% circuits)}
  return(combined_data_f)
}

#' Check for UFLS disconnections
#' Runs UFLS status checks - both voltage and timestamp based.
#' @seealso
#' * [ufls_detection_tstamp()] for detail of timestamp based UFLS detection
#' * [ufls_detection_voltage()] for detail of voltage based UFLS detection
#' @return An updated dataframe with columns for UFLS check results
ufls_detection <- function(
  db_interface, combined_data_f, region_to_load, pre_event_interval, window_length, pre_event_ufls_window_length,
  post_event_ufls_window_length, pre_event_ufls_stability_threshold, post_event_delay
) {
  logdebug("run ufls detection", logger=logger)
  ufls_statuses_ts <- ufls_detection_tstamp(db = db_interface, region = region_to_load,
                                  pre_event_interval = pre_event_interval,
                                  pre_event_window_length = pre_event_ufls_window_length,
                                  post_event_window_length = post_event_ufls_window_length,
                                  pre_pct_sample_seconds_threshold = pre_event_ufls_stability_threshold,
                                  post_event_delay = post_event_delay)

  ufls_statuses_v <- ufls_detection_voltage(combined_data_f, pre_event_interval, window_length, fill_nans = FALSE)
  combined_data_f <- left_join(combined_data_f, ufls_statuses_ts, by = c("c_id"))
  combined_data_f <- left_join(combined_data_f, ufls_statuses_v, by = c("c_id"))
  combined_data_f <- mutate(combined_data_f, response_category =
                            if_else((ufls_status == "UFLS Dropout") |
                                      (ufls_status_v == "UFLS Dropout"),
                                    "UFLS Dropout", response_category, missing=response_category))
  return(combined_data_f)
}

#' Determine distance based zones for each site
#' Distance is from input event location to the postcode the site is in
#' @return An updated dataframe with zones added
determine_distance_zones <- function(
  combined_data_f,
  postcode_data,
  event_latitude,
  event_longitude,
  zone_one_radius,
  zone_two_radius,
  zone_three_radius,
  zones
) {
  if (length(combined_data_f$ts) > 0) {
    combined_data_f <- get_distance_from_event(combined_data_f, postcode_data, event_latitude, event_longitude)
    combined_data_f <- get_zones(combined_data_f, zone_one_radius, zone_two_radius, zone_three_radius)
    combined_data_f <- mutate(combined_data_f,  zone=ifelse(zone %in% c(NA), "NA", zone))
    if (length(zones) < 3) {
      combined_data_f <- filter(combined_data_f, zone %in% zones)
    }
  } else {
    logwarn("no timestamp data found for distance zones", logger=logger)
  }
  return(combined_data_f)
}

#' Normalise power by c_id, based on pre-event interval power
#' @return An updated dataframe with the column c_id_norm_power added
normalise_c_id_power_by_pre_event <- function(combined_data_f, pre_event_interval) {
  logdebug('Calc event normalised power', logger=logger)
  event_powers <- filter(combined_data_f, ts > pre_event_interval - d & ts <= pre_event_interval)
  event_powers <- mutate(event_powers, event_power=power_kW)
  event_powers <- select(event_powers, c_id, clean, event_power)
  combined_data_f <- left_join(combined_data_f, event_powers, by=c("c_id", "clean"))
  combined_data_f <- mutate(combined_data_f, c_id_norm_power=power_kW/event_power)
  return(combined_data_f)
}

#' Normalise power by c_id, based on maximum daily power
#' Optionally adds pre event normalised power as a new column when pre_event_interval is provided
#' @return An updated dataframe with the column c_id_daily_norm_power added
normalise_c_id_power_by_daily_max <- function(combined_data, max_power, pre_event_interval) {
  logdebug('Calc daily normalised power', logger=logger)
  combined_data_f <- left_join(combined_data, max_power, by=c("c_id", "clean"))
  combined_data_f <- mutate(combined_data_f, c_id_daily_norm_power=power_kW/max_power)
  if (!missing(pre_event_interval)) {
    pre_event_daily_norm_power <- filter(combined_data_f, ts > pre_event_interval - d & ts <= pre_event_interval)
    pre_event_daily_norm_power <- mutate(pre_event_daily_norm_power, pre_event_norm_power = c_id_daily_norm_power)
    pre_event_daily_norm_power <- select(pre_event_daily_norm_power, clean, c_id, pre_event_norm_power)
    combined_data_f <- left_join(combined_data_f, pre_event_daily_norm_power, by=c("c_id", "clean"))
  }
  return(combined_data_f)
}

#' Calculate performance factor and pre-event normalised performance factor
#' Performance factor is calculated as power/ac_capacity at a site level
#' @return An updated dataframe with site_performance_factor and event_normalised_performance_factor
determine_performance_factors <- function(combined_data_f, pre_event_interval) {
  logdebug('Calc site peformance factors', logger=logger)
  combined_data_f <- calc_site_performance_factors(combined_data_f)
  combined_data_f <- setnames(combined_data_f, c("ts"), c("Time"))

  combined_data_f <- event_normalised_power(combined_data_f, pre_event_interval, keep_site_id=TRUE)
  combined_data_f <- setnames(
    combined_data_f, c("Event_Normalised_Power_kW"), c("Site_Event_Normalised_Power_kW"))
  combined_data_f <- setnames(combined_data_f, c("Time"), c("ts"))
  return(combined_data_f)
}

#' Upscale disconnections
#' @return A list of data objects summarising disconnections upscaled by manufacturer
upscale_and_summarise_disconnections <- function(circuit_summary, manufacturer_install_data, load_date, region_to_load, exclude_solar_edge) {
  logdebug('Summarise and upscale disconnections on a manufacturer basis.', logger=logger)
  if (exclude_solar_edge) {
    circuits_to_summarise <- filter(circuit_summary, manufacturer != "SolarEdge" | is.na(manufacturer))
    manufacturer_install_data <- filter(manufacturer_install_data, manufacturer != "SolarEdge" | is.na(manufacturer))
  } else {
    circuits_to_summarise <- circuit_summary
    manufacturer_install_data <- manufacturer_install_data
  }
  upscaling_results <- get_upscaling_results(
    circuits_to_summarise,
    manufacturer_install_data,
    load_date,
    region_to_load,
    sample_threshold = 30
  )
  upscaling_results$with_separate_ufls_counts <- get_upscaling_results_excluding_ufls_affected_circuits(
    circuits_to_summarise, manufacturer_install_data, load_date, region_to_load, sample_threshold = 30)

  write.csv(upscaling_results$manufacturers_missing_from_cer,
            "logging/manufacturers_missing_from_cer.csv", row.names=FALSE)
  write.csv(upscaling_results$manufacturers_missing_from_input_db,
            "logging/manufacturers_missing_from_input_db.csv", row.names=FALSE)

  return(upscaling_results)
}

#' Check if grouping is occuring
#' @return boolean no_grouping, TRUE when no grouping is occuring
check_grouping <- function(settings) {
  if (settings$standard_agg==FALSE & settings$pst_agg==FALSE & settings$grouping_agg==FALSE &
      settings$manufacturer_agg==FALSE & settings$model_agg==FALSE & settings$zone_agg==FALSE &
      settings$circuit_agg==TRUE & settings$compliance_agg==TRUE & settings$compliance_2020_agg==TRUE
      & settings$reconnection_compliance_agg & settings$v_excursion_agg==FALSE) {
    no_grouping=TRUE
  } else {
    no_grouping=FALSE
  }
  return(no_grouping)
}

#' Run a complete analysis of the data, based onf the current toll settings
#' data and settings should be provided as list of objects
#' @return list of data with analysis performed, many fields are update or added.
run_analysis <- function(data, settings) {
  errors <- validate_pre_event_interval(
    settings$pre_event_interval,
    settings$load_start_time,
    settings$load_end_time,
    settings$window_length,
    data$combined_data
  )

  if (length(errors$errors) == 0) {
    logging::logdebug("error checks passed", logger=logger)

    # First get ideal response profile for 2015 standard, AS4777.2:2015.
    response_data <- ideal_response_from_frequency(
      data$frequency_data, settings$region_to_load, f_ulco=50.25, f_hyst=0.1, t_hyst=60, f_upper=52.00
    )
    data$ideal_response_to_plot <- response_data$ideal_response_to_plot
    data$region_frequency <- response_data$region_frequency

    # Next, get ideal response profile for 2020 standard, AS4777.2:2020 (uses different settings based on region).
    # Currently, WA (Western Power) uses "Australia B", TAS uses "Australia C", all other NEM regions use "Australia A".
    if(settings$region_to_load == "WA") {
      f_ulco <- 50.15
      f_hyst <- 0.1
      t_hyst <- 20
      f_upper <- 52.00
    } else if(settings$region_to_load == "TAS") {
      f_ulco <- 50.5
      f_hyst <- 0.05
      t_hyst <- 20
      f_upper <- 55.00
    } else{
      f_ulco <- 50.25
      f_hyst <- 0.1
      t_hyst <- 20
      f_upper <- 52.00
    }
    response_data_2020 <- ideal_response_from_frequency(
      data$frequency_data,
      settings$region_to_load,
      f_ulco,
      f_hyst,
      t_hyst,
      f_upper
    )
    data$ideal_response_to_plot_2020 <- response_data_2020$ideal_response_to_plot
    data$region_frequency_2020 <- response_data_2020$region_frequency

    # -------- filter combined data by user filters --------
    combined_data_f <- filter_combined_data(
      data$combined_data,
      data$off_grid_postcodes,
      settings$cleaned,
      settings$size_groupings,
      settings$standards,
      settings$postcodes,
      settings$manufacturers,
      settings$models,
      settings$sites,
      settings$circuits
    )

    # -------- check voltage threshold excursions --------
    combined_data_f <- detect_voltage_threshold_excursions(
      combined_data_f,
      settings$pre_event_interval,
      settings$window_length
    )
    voltage_data_summary <- summarise_voltage_data(combined_data_f)
    combined_data_f <- left_join(combined_data_f, voltage_data_summary, by="c_id")
    data$antiislanding_summary <- antiislanding_summary(combined_data_f)

    if (length(combined_data_f$ts) > 0) {
      # -------- categorise response --------
      combined_data_f <- categorise_response(
        combined_data_f, settings$pre_event_interval, settings$window_length, settings$NED_threshold)
      combined_data_f <- mutate(combined_data_f,
                                response_category = ifelse(response_category %in% c(NA), "NA", response_category))
      combined_data_f <- filter(combined_data_f, response_category %in% settings$responses)

      combined_data_f <- ufls_detection(
        data$db, combined_data_f, settings$region_to_load, settings$pre_event_interval, settings$window_length,
        settings$pre_event_ufls_window_length, settings$post_event_ufls_window_length,
        settings$pre_event_ufls_stability_threshold, as.numeric(settings$duration)
      )

      # process alerts where they exist
      alert_data <- data$db$get_alerts_data()
      if (length(alert_data$c_id) > 0) {
        # run islanded site assessment
        combined_data_f <- classify_islands(
          combined_data_f,
          alert_data,
          settings$pre_event_interval,
          settings$window_length
        )
      }
    }

    if ("Islanded" %in% names(combined_data_f) & settings$exclude_islanded_circuits) {
      combined_data_f <- filter(combined_data_f, !Islanded)
      combined_data_f <- subset(combined_data_f, select = -c(Islanded, island_assessment, islanding_alert))
      data$antiislanding_summary <- antiislanding_summary(combined_data_f)
    } else if ("Islanded" %in% names(combined_data_f)) {
      combined_data_f <- mutate(combined_data_f, response_category=ifelse(island_assessment %in%
              c("Frequency disruption", "Voltage disruption", "Gateway curtailed"), "Undefined", response_category))
    }

    combined_data_f <- determine_distance_zones(
      combined_data_f, data$postcode_data, settings$event_latitude, settings$event_longitude, settings$zone_one_radius,
      settings$zone_two_radius, settings$zone_three_radius, settings$zones
    )

    # -------- filter by time window --------
    logdebug('filter by time window', logger=logger)
    if (length(settings$offsets) < length(data$unique_offsets)) {
      combined_data_f <- filter(combined_data_f, time_offset %in% settings$offsets)
    }

    combined_data_f <- normalise_c_id_power_by_pre_event(combined_data_f, settings$pre_event_interval)

    if (length(combined_data_f$ts) > 0) {
      combined_data_f <- determine_performance_factors(combined_data_f, settings$pre_event_interval)

      # -------- determine AS4777.2:2015 over-frequency f-W compliance --------
      if (dim(data$ideal_response_to_plot)[1]>0) {
        ideal_response_downsampled <- down_sample_1s(
          data$ideal_response_to_plot, settings$duration, min(combined_data_f$ts))
        data$ideal_response_downsampled <- ideal_response_downsampled
        combined_data_f <-
          calc_error_metric_and_compliance_2(combined_data_f,
                                              ideal_response_downsampled,
                                              data$ideal_response_to_plot,
                                              settings$compliance_threshold,
                                              settings$start_buffer,
                                              settings$end_buffer,
                                              settings$end_buffer_responding,
                                              settings$disconnecting_threshold)
        combined_data_f <- mutate(
          combined_data_f,  compliance_status=ifelse(compliance_status %in% c(NA), "NA", compliance_status))
      } else {
        combined_data_f <- mutate(combined_data_f, compliance_status="Undefined")
      }
      if (length(settings$compliance) < 8) {
        combined_data_f <- filter(combined_data_f, compliance_status %in% settings$compliance)
      }

      # -------- determine AS4777.2:2020 over-frequency f-W compliance --------
      if (dim(data$ideal_response_to_plot_2020)[1]>0) {
        ideal_response_downsampled_2020 <- down_sample_1s(
          data$ideal_response_to_plot_2020, settings$duration, min(combined_data_f$ts))
        data$ideal_response_downsampled_2020 <- ideal_response_downsampled_2020
        combined_data_f <-
          calc_error_metric_and_compliance_2(combined_data_f,
                                             ideal_response_downsampled_2020,
                                             data$ideal_response_to_plot_2020,
                                             settings$compliance_threshold_2020,
                                             settings$start_buffer_2020,
                                             settings$end_buffer_2020,
                                             settings$end_buffer_responding_2020,
                                             settings$disconnecting_threshold)
        combined_data_f <- mutate(
          combined_data_f,  compliance_status_2020=ifelse(compliance_status_2020 %in% c(NA), "NA", compliance_status_2020))
      } else {
        combined_data_f <- mutate(combined_data_f, compliance_status_2020="Undefined")
      }
      if (length(settings$compliance_2020) < 8) {
        combined_data_f <- filter(combined_data_f, compliance_status_2020 %in% settings$compliance_2020)
      }

      # -------- determine reconnection compliace --------
      logdebug('Set reconnection compliance values', logger=logger)
      max_power <- data$db$get_max_circuit_powers(settings$region_to_load)
      combined_data_f <- normalise_c_id_power_by_daily_max(
        combined_data_f, max_power, settings$pre_event_interval
      )
      event_window_data <- filter(combined_data_f, ts > settings$pre_event_interval - d)
      reconnection_categories <- create_reconnection_summary(event_window_data, settings$pre_event_interval,
                                                              settings$disconnecting_threshold,
                                                              reconnect_threshold = settings$reconnection_threshold,
                                                              ramp_rate_threshold = settings$ramp_rate_threshold,
                                                              ramp_threshold_for_compliance =
                                                                settings$total_ramp_threshold_for_compliance,
                                                              ramp_threshold_for_non_compliance =
                                                                settings$total_ramp_threshold_for_non_compliance,
                                                              ramp_rate_change_resource_limit_threshold =
                                                                settings$ramp_rate_change_resource_limit_threshold)
      combined_data_f <- left_join(combined_data_f, reconnection_categories, by = 'c_id')

      # -------- calculate summary stats --------
      logdebug('Count samples in each data series to be displayed', logger=logger)
      grouping_cols <- find_grouping_cols(settings)

      data$sample_count_table <- vector_groupby_count(combined_data_f, grouping_cols)
      if (settings$confidence_category %in% grouping_cols) {
        population_groups <- grouping_cols[settings$confidence_category != grouping_cols]
        population_count_table <- vector_groupby_count(combined_data_f, population_groups)
        population_count_table <- dplyr::rename(population_count_table, sub_population_size=sample_count)
        data$sample_count_table <- left_join(population_count_table, data$sample_count_table, by=population_groups)
        data$sample_count_table$percentage_of_sub_pop <- data$sample_count_table$sample_count / data$sample_count_table$sub_population_size
        data$sample_count_table$percentage_of_sub_pop <- round(data$sample_count_table$percentage_of_sub_pop, digits = 4)
        result <- mapply(confidence_interval, data$sample_count_table$sample_count,
                          data$sample_count_table$sub_population_size, 0.95)
        data$sample_count_table$lower_95_CI <- round(result[1,], digits = 4)
        data$sample_count_table$upper_95_CI <- round(result[2,], digits = 4)
        data$sample_count_table$width <- data$sample_count_table$upper_95_CI - data$sample_count_table$lower_95_CI
      }
    }

    no_grouping <- check_grouping(settings)

    # Proceed to aggregation and plotting only if there is less than 1000 data series to plot, else stop and notify the
    # user.
    logdebug('Proceed to aggregation and plotting', logger=logger)
    if ((sum(data$sample_count_table$sample_count)<1000 & no_grouping) |
        (length(data$sample_count_table$sample_count)<1000 & !no_grouping)) {
      if (length(combined_data_f$ts) > 0) {
        # Copy data for saving
        logdebug('Copy data for saving', logger=logger)
        combined_data_cols <- c("ts", "site_id", "c_id", "power_kW", "c_id_norm_power", "v", "vmin", "vmax", "vmean", "f", "s_state",
                                "s_postcode", "pv_installation_year_month", "Standard_Version", "Grouping", "sum_ac",
                                "clean", "manufacturer", "model", "site_performance_factor", "response_category",
                                "zone", "distance", "lat", "lon", "e", "con_type", "first_ac", "polarity",
                                "compliance_status","compliance_status_2020", "reconnection_compliance_status",
                                "manual_droop_compliance", "manual_reconnect_compliance", "reconnection_time",
                                "ramp_above_threshold", "c_id_daily_norm_power", "max_power", "ufls_status",
                                "pre_event_sampled_seconds", "post_event_sampled_seconds", "ufls_status_v",
                                "pre_event_v_mean", "post_event_v_mean", "vmin_na", "vmax_na", "vmean_na",
                                "antiislanding_v_excursion_2015", "antiislanding_v_excursion_2020")
        if ("Islanded" %in% names(combined_data_f)) {
          combined_data_cols <- append(combined_data_cols, c("Islanded", "island_assessment", "islanding_alert"), 34)
        }
        data$combined_data_f <- combined_data_f[, combined_data_cols]

        # Create copy of filtered data to use in upscaling
        combined_data_f2 <- combined_data_f
          if (settings$raw_upscale) {
            combined_data_f2 <- upscale(combined_data_f2, data$install_data)
          }
      }

      # Check that the filter does not result in an empty dataframe.
      logdebug('Check that the filter does not result in an empty dataframe.', logger=logger)
      if (length(combined_data_f$ts) > 0) {

        # -------- Initialise aggregate dataframes  --------
        if (settings$norm_power_filter_off_at_t0) {
          combined_data_for_norm_power <- filter(combined_data_f,  response_category != "5 Off at t0")
        } else {
          combined_data_for_norm_power <- combined_data_f
        }
        data$agg_norm_power <- vector_groupby_norm_power(combined_data_for_norm_power, grouping_cols)
        agg_f_and_v <- vector_groupby_f_and_v(combined_data_f, grouping_cols)
        data$agg_power <- vector_groupby_power(combined_data_f2, grouping_cols)
        data$response_count <- vector_groupby_count_response(combined_data_f, grouping_cols)
        data$zone_count <- vector_groupby_count_zones(combined_data_f, grouping_cols)
        data$distance_response <- vector_groupby_cumulative_distance(combined_data_f, grouping_cols)
        data$geo_data <- vector_groupby_system(combined_data_f, grouping_cols)
        data$circuit_summary <- distinct(combined_data_f, c_id, clean, .keep_all = TRUE)
        circ_sum_cols <- c(
          "site_id",
          "c_id",
          "s_state",
          "s_postcode",
          "pv_installation_year_month",
          "Standard_Version",
          "Grouping",
          "sum_ac",
          "clean",
          "manufacturer",
          "model",
          "response_category",
          "zone",
          "distance",
          "lat",
          "lon",
          "con_type",
          "first_ac",
          "polarity",
          "compliance_status",
          "compliance_status_2020",
          "reconnection_compliance_status",
          "manual_droop_compliance",
          "manual_reconnect_compliance",
          "reconnection_time",
          "ramp_above_threshold",
          "max_power",
          "ufls_status",
          "pre_event_sampled_seconds",
          "post_event_sampled_seconds",
          "ufls_status_v",
          "pre_event_v_mean",
          "post_event_v_mean",
          "vmax_max",
          "vmin_min",
          "vmean_mean",
          "vmin_na_all",
          "vmax_na_all",
          "vmean_na_all",
          "antiislanding_v_excursion_2015_triggered",
          "antiislanding_v_excursion_2020_triggered"
        )
        if ("Islanded" %in% names(data$circuit_summary)) {
          circ_sum_cols <- append(circ_sum_cols, c("Islanded", "island_assessment", "islanding_alert"), 26)
        }
        data$circuit_summary <- data$circuit_summary[, circ_sum_cols]
        if (is.null(git2r::discover_repository(path = ".", ceiling = NULL))) {
          data$circuit_summary$tool_hash <- Sys.Date()
        } else {
          data$circuit_summary$tool_hash <-git2r::revparse_single(revision="HEAD")$sha
        }

        # Combine data sets that have the same grouping so they can be saved in a single file
        if (no_grouping) {
          #et <- settings$pre_event_interval
          # agg_norm_power <- event_normalised_power(agg_norm_power, et, keep_site_id=TRUE)
          data$agg_power <- left_join(data$agg_power, data$agg_norm_power[, c("c_id_norm_power", "c_id", "Time")],
                                    by=c("Time", "c_id"))
        } else {
          #et <- settings$pre_event_interval
          # agg_norm_power <- event_normalised_power(agg_norm_power, et,  keep_site_id=FALSE)
          data$agg_power <- left_join(data$agg_power, data$agg_norm_power[, c("c_id_norm_power", "series", "Time")],
                                    by=c("Time", "series"))
        }
        data$agg_power <- left_join(data$agg_power, agg_f_and_v[, c("Time", "series", "Voltage", "Frequency")],
                                  by=c("Time", "series"))

        # Summarise and upscale disconnections on a manufacturer basis.
        upscaling_results <- upscale_and_summarise_disconnections(
          data$circuit_summary, data$manufacturer_install_data, settings$load_date, settings$region_to_load, settings$exclude_solar_edge
        )
        data$disconnection_summary <- upscaling_results$disconnection_summary
        data$upscaled_disconnections <- upscaling_results$upscaled_disconnections
        data$disconnection_summary_with_separate_ufls_counts <-
          upscaling_results$with_separate_ufls_counts$disconnection_summary
        data$upscaled_disconnections_with_separate_ufls_counts <-
          upscaling_results$with_separate_ufls_counts$upscaled_disconnections

        if (length(upscaling_results$manufacturers_missing_from_cer$manufacturer) > 0 |
           length(upscaling_results$manufacturers_missing_from_input_db$manufacturer) > 0) {
          num_missing_from_cer <- length(upscaling_results$manufacturers_missing_from_cer$manufacturer)
          num_missing_from_input_db <- length(upscaling_results$manufacturers_missing_from_input_db$manufacturer)
          long_error_message <- c("%s manufacturers present in the input data could not be ",
                                  "matched to the CER data set. \n%s manufacturers present in the CER data could not be ",
                                  "matched to the input data set. \nLists of each of these have been saved in the ",
                                  "files logging/manufacturers_missing_from_[dataset].csv. You may want to review the ",
                                  "mapping used in processing the input data and check the number and names of ",
                                  "missing manufacturers.")
          long_error_message <- sprintf(paste(long_error_message, collapse = ''), num_missing_from_cer,
                                        num_missing_from_input_db)
          errors$warnings[[length(errors$warnings) + 1]] <- list(title="Manufacturers missing from datasets",
                                                                 body=long_error_message)
        }
      }
    }
  }
  results <- list()
  results$data <- data
  results$errors <- errors
  return(results)
}
