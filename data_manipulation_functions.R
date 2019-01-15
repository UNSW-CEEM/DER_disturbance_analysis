
process_raw_time_series_data <- function(time_series_data){
  t0 <- as.numeric(Sys.time())
  time_series_data <- time_series_data[!time_series_data$c_id=="c_id",]
  print("suppressWarnings")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  time_series_data <- time_series_data %>%
    mutate(ts = fastPOSIXct(ts))
  print("ymd_hms")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  time_series_data <- time_series_data %>%
    mutate(ts = with_tz(ts,"Australia/Brisbane"))
  print("with_tz")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  time_series_data$d[is.na(time_series_data$d)] <- "5"
  print("d")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  time_series_data <- time_series_data %>%  
    mutate(d = as.numeric(d))
  print("d")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  time_series_data <- time_series_data %>%
    mutate(c_id = as.numeric(c_id))
  print("c_id")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  processed_time_series_data <- time_series_data %>%
    mutate(e = as.numeric(e))
  print("e")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  return(processed_time_series_data)
}

process_raw_circuit_details <- function(circuit_details){
  site_types <- c("pv_site_net", "pv_site", "pv_inverter_net")
  processed_circuit_details <- filter(circuit_details, con_type %in% site_types)
  return(processed_circuit_details)
}

process_raw_site_details <- function(site_details){
  site_details <- site_details %>% 
    group_by(site_id)
  processed_site_details <- site_details %>%
    summarise(s_state=first(s_state), 
              pv_installation_year_month=first(pv_installation_year_month),
              sum_dc=sum(dc), first_ac=first(ac))
  processed_site_details <- as.data.frame(processed_site_details)
  processed_site_details <- filter(processed_site_details, sum_dc<=100000)
  return(processed_site_details)}

perform_power_calculations <- function(master_data_table){
  master_data_table <- master_data_table %>%
    mutate(e_polarity=e*polarity)
  master_data_table <- master_data_table %>%
    mutate(power_kW = e_polarity/(d * 1000))
  return(master_data_table)
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
  t0 <- as.numeric(Sys.time())
  print("process_raw_time_series_data")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  circuit_details <- process_raw_circuit_details(circuit_details)
  print("process_raw_circuit_details")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  print("process_raw_site_details")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  site_details <- site_catergorisation(site_details)
  print("site_catergorisation")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  time_series_data <- inner_join(time_series_data, circuit_details, by="c_id")
  print("left_join")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  time_series_data <- inner_join(time_series_data, site_details, by="site_id")
  print("left_join")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  time_series_data <- perform_power_calculations(time_series_data)
  print("perform_power_calculations")
  print(as.numeric(Sys.time()) - t0, digits=15)
  t0 <- as.numeric(Sys.time())
  print("implicit_filtering")
  print(as.numeric(Sys.time()) - t0, digits=15)
  return(time_series_data)
}