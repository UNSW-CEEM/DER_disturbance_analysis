
process_raw_time_series_data <- function(time_series_data){
  suppressWarnings(
    time_series_data <- time_series_data[
      !is.na(as.numeric(time_series_data$c_id)),])
  processed_time_series_data <- time_series_data %>%
    mutate(ts = ymd_hms(ts,tz = "Etc/UTC")) %>% 
    mutate(ts = with_tz(ts,"Australia/Brisbane"))%>% 
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
  v1 <- aggregate(s_state ~ site_id, data = site_details, first)
  #v2 <- aggregate(pv_installation_year_month ~ site_id, data = site_details, 
  #                first)
  v2 <- aggregate(dc ~ site_id, data = site_details, sum)
  processed_site_details <- merge(v1, v2, by=c("site_id"))
  #processed_site_details <- merge(processed_site_details, v2, by=c("site_id"))
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

site_catergorisation <- function(combined_data){
  combined_data <- combined_data %>%
    mutate(pv_installation_year_month=
             ifelse(pv_installation_year_month=="", "2005-01", 
                    pv_installation_year_month)) %>% 
    mutate(pv_installation_year_month=
             ifelse(is.na(pv_installation_year_month), 
                    "2005-01", pv_installation_year_month)) %>% 
    mutate(pv_installation_year_month=
             ifelse(nchar(pv_installation_year_month)==10,
                    pv_installation_year_month, 
                    paste0(pv_installation_year_month, "-28"))) %>% 
    mutate(pv_installation_year_month=
             ymd(pv_installation_year_month)) %>% 
    mutate(Standard_Version= 
             ifelse(pv_installation_year_month<"2015-10-09", "AS4777.3:2005", 
                    ifelse(pv_installation_year_month>="2016-10-09", 
                           "AS4777.2:2015", "Transition")))
  return(combined_data)
}
  
combine_data_tables <- function(time_series_data, circuit_details, 
                                site_details) {
  time_series_data <- process_raw_time_series_data(time_series_data)
  circuit_details <- process_raw_circuit_details(circuit_details)
  site_details <- process_raw_site_details(site_details)
  time_series_data <- left_join(time_series_data, circuit_details, by="c_id") 
  time_series_data <- left_join(time_series_data, site_details, by="site_id")
  time_series_data <- perform_power_calculations(time_series_data)
  master_data_table <- implicit_filtering(time_series_data)
  return(master_data_table)
}