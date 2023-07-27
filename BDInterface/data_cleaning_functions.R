calc_max_kw_per_site <- function(performance_data){
  performance_data <- group_by(performance_data, ts, site_id)
  performance_data <- summarise(performance_data , power_kW=sum(power_kW, na.rm = TRUE))
  performance_data <- as.data.frame(performance_data)
  performance_data <- group_by(performance_data, site_id)  
  site_max_power <- summarise(performance_data , max_power_kW=max(power_kW, na.rm = TRUE))
  site_max_power <- as.data.frame(site_max_power)
  site_max_power <- site_max_power[,c("site_id", "max_power_kW")]
  return(site_max_power)
}

sum_manufacturers <- function(manufacturers){
  unique_manufactuerers <- unique(manufacturers)
  if (anyNA(unique_manufactuerers)) {
    manufacturer <- 'NA'
  } else if (length(unique_manufactuerers) > 1) {
    manufacturer <- 'Mixed' 
  } else {
    manufacturer <- unique_manufactuerers[1]
  }
  return(manufacturer)
}

group_site_details_to_one_row_per_site <- function(site_details){
  site_details <- site_details %>% group_by(site_id)
  processed_site_details <- site_details %>%
    summarise(s_state=first(s_state), 
              pv_installation_year_month=first(pv_installation_year_month),
              sum_ac=sum(ac), sum_dc=sum(dc),
              sum_ac_old=sum(ac_old), sum_dc_old=sum(dc_old),
              ac_dc_ratio=mean(ac_dc_ratio),
              manufacturer= sum_manufacturers(manufacturer),
              model=paste(model, collapse=' '), s_postcode=first(s_postcode),
              rows_grouped=length(ac))
  processed_site_details <- as.data.frame(processed_site_details)
  return(processed_site_details)
}

site_details_data_cleaning_one <- function(site_details){
  # Record old ac and dc values.
  site_details <- mutate(site_details, ac_old=ac)
  site_details <- mutate(site_details, dc_old=dc)
  site_details <- check_for_ac_value_in_watts(site_details)
  # Find peak power per site for more checks
  site_details <- group_site_details_to_one_row_per_site(site_details)
  return(site_details)
}


site_details_data_cleaning_two <- function(time_series, site_details){
  max_site_power <- calc_max_kw_per_site(time_series)
  site_details <- left_join(site_details, max_site_power, by=c("site_id"))
  site_details <- check_for_peak_power_greater_than_dc_capacity(site_details)
  # Record if dc value was scaled or not.
  site_details <- mutate(site_details, change_dc=ifelse(sum_dc!=sum_dc_old,1,0))
  site_details <- check_ac_capacity_less_than_peak_power(site_details)
  # record if ac acpacity value was changed.
  site_details <- mutate(site_details, change_ac=ifelse(sum_ac!=sum_ac_old,1,0))
  
  site_details <- data.table::setnames(site_details, c("sum_ac", "sum_dc", "sum_ac_old", "sum_dc_old"), 
                                       c("ac", "dc", "ac_old", "dc_old"))
  return(site_details)
}

check_for_peak_power_greater_than_dc_capacity <- function(site_details){
  # If dc capacity value doesn't make sense given peak power value then scale
  # dc capacity up, based on nearest interger scaling value that makes dc
  # capacity higher than peak power.
  site_details <- mutate( site_details, 
                          sum_dc=ifelse((sum_dc/1000)/abs(max_power_kW) < 0.9, 
                                        ceiling(max_power_kW/(sum_dc/1000)) * sum_dc, sum_dc))
  return(site_details)
}

check_for_ac_value_in_watts <- function(site_details){
  # Calculate helper value
  site_details <- mutate(site_details, ac_dc_ratio=ac/dc)
  # Check if ac value is in watts, if so devide by 1000.
  site_details <- mutate(site_details, ac=ifelse(ac_dc_ratio > 0.1 & ac > 150, ac/1000, ac))
  return(site_details)
}

check_ac_capacity_less_than_peak_power <- function(site_details){
  # If ac value needs scaling to based on ratio to peak power and dc has not
  # been scaled, then scale to meet dc capacity, if dc was also scaled then 
  # scale to meet peak power value.
  site_details <- mutate(site_details, 
                         sum_ac=ifelse(sum_ac/abs(max_power_kW) < 0.6, 
                                       ifelse(change_dc==0,ceiling((sum_dc/1000)/sum_ac) * sum_ac, 
                                              ceiling(max_power_kW/sum_ac) * sum_ac), 
                                       sum_ac))
  return(site_details)
}

clean_connection_types <- function(combined_data, circuit_details, postcode_data){
  postcode_data <- calc_sunrise_sunset_bounds(postcode_data, as.Date(combined_data$ts[1]))
  # Merge sunrise and sunset times onto the timeseries data frame.
  combined_data <- left_join(combined_data, select(postcode_data, postcode, sunrise, sunset, dis_sunrise, dis_sunset), 
                             by=c("s_postcode" = "postcode"))
  # Calculate the values needed to determine if a connection type is correct
  combined_data <- clac_output_summary_values(combined_data)
  # Record details before changes
  combined_data <- mutate(combined_data, old_con_type=con_type)
  combined_data <- mutate(combined_data, old_polarity=polarity)
  combined_data <- check_day_vs_night_energy(combined_data)
  combined_data <- check_for_reversed_polarity(combined_data)
  combined_data <- check_for_mixed_polarity(combined_data)
  # Flag systems with changed connection type or polarity.
  combined_data <- mutate(combined_data, con_type_changed=ifelse(con_type!=old_con_type,1,0))
  combined_data <- mutate(combined_data, polarity_changed=ifelse(polarity!=old_polarity,1,0))
  # Remove first_ac column
  combined_data <- combined_data[ , -which(names(combined_data) %in% c("ac"))]
  # Select the values from the orginal circuit details that would not be changed 
  # by cleaning, then merge back in with details updated or created by cleaning
  circuit_details <- select(circuit_details, site_id, c_id, manual_droop_compliance, manual_reconnect_compliance)
  combined_data <- left_join(combined_data, circuit_details, by="c_id")
  return(combined_data)
}

calc_sunrise_sunset_bounds <- function(postcode_data, event_date){
  # Need date of event to calaculate sunrise and sunset times.
  postcode_data$date <- event_date
  # Find sunrise and sunset times on a postcode basis.
  postcode_data <- mutate(postcode_data, sunrise=suncalc::getSunlightTimes(data=postcode_data, tz="Australia/Brisbane", 
                                                                  keep=c('sunrise'))$sunrise)
  postcode_data <- mutate(postcode_data, sunset=suncalc::getSunlightTimes(data=postcode_data, tz="Australia/Brisbane",
                                                                 keep=c('sunset'))$sunset)
  # Create 1 hour buffer either side of sunrise and sunset to allow for large 
  # postcodes, as lat and lon is the postcode centre.
  postcode_data <- mutate(postcode_data, sunrise=sunrise-60*60)
  postcode_data <- mutate(postcode_data, sunset=sunset+60*60)
  # Format sunrise and sunset as character so it is displayed in brisbane time in gui.
  postcode_data <- mutate(postcode_data, dis_sunrise=strftime(sunrise, tz="Australia/Brisbane", format="%H:%M:%S"))
  postcode_data <- mutate(postcode_data, dis_sunset=strftime(sunset, tz="Australia/Brisbane", format="%H:%M:%S"))
  return(postcode_data)
}

clac_output_summary_values <- function(combined_data){
  # Determine if a data point is during daylight hours or not.
  combined_data <- mutate(combined_data, comp_t=strftime(ts, tz="Australia/Brisbane", format="%H:%M:%S"))
  combined_data <- mutate(combined_data, sunrise=strftime(sunrise, tz="Australia/Brisbane", format="%H:%M:%S"))
  combined_data <- mutate(combined_data, sunset=strftime(sunset, tz="Australia/Brisbane", format="%H:%M:%S"))
  combined_data <- mutate(combined_data, daylight=ifelse(comp_t>sunrise & comp_t<sunset,1,0))
  # Group data by c_id, calculating values needed to perform data cleaning
  combined_data <- group_by(combined_data, c_id)
  combined_data <- summarise(combined_data, energy_day=sum(abs(e[daylight==1]))/1000/(60*60), 
                             energy_night=sum(abs(e[daylight!=1]))/1000/(60*60),
                             con_type=first(con_type), sunrise=first(dis_sunrise), 
                             sunset=first(dis_sunset), ac=first(ac),
                             min_power=min(power_kW, na.rm = TRUE), max_power=max(power_kW, na.rm = TRUE), 
                             polarity=first(polarity))
  combined_data <- as.data.frame(combined_data)
  combined_data <- mutate(combined_data, energy_day=ifelse(is.na(energy_day), 0.0, energy_day))
  combined_data <- mutate(combined_data, energy_night=ifelse(is.na(energy_night), 0.0, energy_night))
  # Calculate the fraction of gen/load that occured during daylight hours, just
  # use absolute value of power as polarity has not been checked yet.
  combined_data <- mutate(combined_data, frac_day=round(energy_day/(energy_day+energy_night), digits=2))
}

check_day_vs_night_energy <- function(combined_data){
  # Check for pv connection type, if the type is pv but more than 25% of the 
  # gen/load was outside daylight hours then change to type 'load'. Only change
  # if system has operated at above an avergae of 1% capacity
  combined_data <- mutate(combined_data, 
                          con_type=ifelse(frac_day<0.75 & con_type %in% c("pv_site_net", "pv_site", "pv_inverter_net") &
                                            (energy_day+energy_night) > ac * 24 * 0.01, "load", con_type))
  return(combined_data)
}

check_for_reversed_polarity <- function(combined_data){
  # Check for peak power occuring as a negative value i.e. reversed polarity,
  # if this occurs for pv systems that have operated at above an avergae of 1%
  # capacity then swap polarity.
  combined_data <- mutate(
    combined_data,
    polarity=ifelse(abs(min_power) > abs(max_power) & con_type %in% c("pv_site_net", "pv_site", "pv_inverter_net") & 
                      (energy_day+energy_day) > ac * 24 * 0.01 & min_power < 0.0, polarity * -1, polarity))
  return(combined_data)
}

check_for_mixed_polarity <- function(combined_data){
  # Check for power flowing in both negative and positive direction, if this 
  # occurs with values large than 10 % of the inverter capacity then change 
  # connection type to 'mixed'
  combined_data <- mutate(combined_data, con_type=ifelse(max_power > ac * .1 & min_power * -1 > ac * 0.1, 
                                                         "mixed", con_type))
  return(combined_data)
}

voltages_in_bounds <- function(v) {
  MIN_VOLTAGE <- 10
  MAX_VOLTAGE <- 1000

  return(sum(v > MIN_VOLTAGE & v < MAX_VOLTAGE, na.rm=TRUE))
}

#' Replace outlier voltages with NA to avoid corrupting results
#' 
#' Only replaces voltages where ALL recorded voltages are outside bounds
remove_outlying_voltages <- function(time_series) {
  voltage_cols <- c("v", "vmin", "vmax", "vmean")
  time_series[, voltage_cols] <- sapply(time_series[, voltage_cols], as.numeric)
  old_time_series <- time_series

  voltage_extremes <- time_series %>%
    select(c("c_id"), voltage_cols) %>%
    group_by(c_id) %>%
    summarise_all(voltages_in_bounds)
  
  voltage_extremes[, voltage_cols][voltage_extremes[, voltage_cols] > 0] <- 1
  voltage_extremes[, voltage_cols][voltage_extremes[, voltage_cols] == 0] <- NaN

  time_series[, voltage_cols] <- (
    time_series[, voltage_cols] *
    voltage_extremes[match(time_series$c_id, voltage_extremes$c_id), voltage_cols]
  )
  time_series['v_changed'] <- rowSums(
    is.na(time_series[, voltage_cols]) & !is.na(old_time_series[, voltage_cols])
  ) > 0
  return(time_series)
}
