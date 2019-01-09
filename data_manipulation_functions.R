
process_raw_time_series_data <- function(time_series_data, start_time, end_time){
  processed_time_series_data <- time_series_data %>%
    mutate(ts = ymd_hms(ts,tz = "Etc/UTC")) %>% 
    mutate(ts = with_tz(ts,"Australia/Brisbane"))%>% 
    filter(ts>=start_time & ts<=end_time) %>%
    mutate(d = ifelse(is.na(d), 5, d)) %>%
    mutate(d = as.numeric(d))%>%
    mutate(c_id = as.numeric(c_id))%>%
    mutate(e = as.numeric(e))
  return(processed_time_series_data)
}

process_raw_circuit_details <- function(circuit_details){
  processed_circuit_details <- circuit_details
  return(processed_circuit_details)
}

process_raw_site_details <- function(site_details){
  v1 <- aggregate( s_state ~ site_id, data = site_details, first)
  v2 <- aggregate( dc ~ site_id, data = site_details, sum)
  processed_site_details <- merge(v1, v2, by=c("site_id"))
  return(processed_site_details)}

perform_power_calculations <- function(master_data_table){
  master_data_table <- master_data_table %>%
    mutate(e_polarity=e*polarity)
  master_data_table <- master_data_table %>%
    mutate(power_kW = e_polarity/(d * 1000))
  return(master_data_table)
}

implicit_filtering <- function(time_series_data){
  site_types <- c("pv_site_net", "pv_site", "pv_inverter_net")
  time_series_data <- filter(time_series_data, con_type %in% site_types)
  time_series_data <- filter(time_series_data, dc<=100000)
  return(time_series_data)
}

explicit_filtering <- function(master_data_table, sample_rate, regions){
  time_series_data <- filter(time_series_data, d==sample_rate)
  time_series_data <- filter(time_series_data, s_state %in% regions)
}
  
combine_data_tables <- function(time_series_data, circuit_details, site_details, start_time, end_time) {
  time_series_data <- process_raw_time_series_data(time_series_data, start_time, end_time)
  circuit_details <- process_raw_circuit_details(circuit_details)
  site_details <- process_raw_site_details(site_details)
  time_series_data <- left_join(time_series_data, circuit_details, by="c_id") 
  time_series_data <- left_join(time_series_data, site_details, by="site_id")
  time_series_data <- perform_power_calculations(time_series_data)
  master_data_table <- implicit_filtering(time_series_data)
  return(master_data_table)
}