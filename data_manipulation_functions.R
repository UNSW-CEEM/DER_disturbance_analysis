
process_raw_time_series_data <- function(time_series_data){
  # Sometimes data from solar analytics contains rows that are additional 
  # headers explicity remove these.
  time_series_data <- time_series_data[!time_series_data$c_id=="c_id",]
  # Convert time stamp to date time object assuming UTC time is being used.
  time_series_data <- time_series_data %>% 
    mutate(ts = fastPOSIXct(ts))
  # Change time zone to NEM standard time.
  time_series_data <- time_series_data %>%
    mutate(ts = with_tz(ts,"Australia/Brisbane"))
  # If a data record does not have a specified duration assume it is equal to 
  # 5 s.
  time_series_data$d[is.na(time_series_data$d)] <- "5"
  # Convert data that comes as strings to numeric where applicable.
  time_series_data <- time_series_data %>%  
    mutate(d = as.numeric(d))
  time_series_data <- time_series_data %>%
    mutate(c_id = as.numeric(c_id))
  processed_time_series_data <- time_series_data %>%
    mutate(e = as.numeric(e))
  return(processed_time_series_data)
}

process_raw_circuit_details <- function(circuit_details){
  # Filter circuit id by connection type, just including solar data.
  site_types <- c("pv_site_net", "pv_site", "pv_inverter_net")
  processed_circuit_details <- filter(circuit_details, con_type %in% site_types)
  return(processed_circuit_details)
}

process_raw_site_details <- function(site_details){
  # Older site details proided the day of installation not just the month. We 
  # change the name of the column to match the new format which is just by 
  # month but keep the original info regarding the date.
  if("pv_install_date" %in% colnames(site_details)){
    data <- setnames(site_details, c("pv_install_date"),
                     c("pv_installation_year_month"))
  }
  # The data can contain duplicate site ids, these need to be sumarised so there
  # is one row per site id. DC power is summed so sites with more than 100kW 
  # can be filtered out of the data set. The first ac value is taken as a sample
  # of the site, it is not summed, as latter when the data is joined to the 
  # circuit data (which may have mutiple rows be site_id) this would create
  # apparent additional ac capacity.
  site_details <- site_details %>% 
    group_by(site_id)
  processed_site_details <- site_details %>%
    summarise(s_state=first(s_state), 
              pv_installation_year_month=first(pv_installation_year_month),
              sum_ac=sum(ac), first_ac=first(ac))
  processed_site_details <- as.data.frame(processed_site_details)
  processed_site_details <- filter(processed_site_details, sum_ac<=100)
  return(processed_site_details)}

perform_power_calculations <- function(master_data_table){
  # Calculate the average power output over the sample time base on the 
  # cumulative energy and duration length.
  master_data_table <- master_data_table %>%
    mutate(e_polarity=e*polarity)
  master_data_table <- master_data_table %>%
    mutate(power_kW = e_polarity/(d * 1000))
  return(master_data_table)
}

process_install_data <- function(install_data){
  # Rename time column and catergorise data based on inverter standards.
  install_data <- install_data %>% mutate(pv_installation_year_month=index)
  install_data <- site_catergorisation(install_data)
  # Convert column names to same format as time solar analytics data.
  install_data <- setnames(install_data, c("pv_installation_year_month", "State"),
                           c("date", "s_state"))
  # Reset the intial installed capacity to zero, this is need so the first 
  # inverter standard has a correct reference point for calculating its current
  # install capacity as the data is in cumulative format.
  start_date <- min(install_data$date)
  install_data <- install_data %>% 
    mutate(Capacity = ifelse(date==start_date, 0, Capacity))
  # For each inverter standard group find the intall capacity when the standard
  # came into force.
  installed_start_standard <- group_by(install_data, Standard_Version, Grouping, 
                                      s_state)
  installed_start_standard <- summarise(installed_start_standard, 
       initial_cap=ifelse(min(Capacity) >= 0.0001,
         install_data$Capacity[((install_data$date[abs(install_data$Capacity - min(Capacity)) <= 0.01 
                             & install_data$Standard_Version == first(Standard_Version)
                             & install_data$s_state == first(s_state)
                             & install_data$Grouping == first(Grouping)])[1]-1) == install_data$date
         & install_data$s_state == first(s_state)
         & install_data$Grouping == first(Grouping)], 0))
  installed_start_standard <- as.data.frame(installed_start_standard)
  # Join the intial capacity to the cumulative capacity table.
  install_data <- inner_join(install_data, installed_start_standard, 
                             by=c("Standard_Version", "Grouping", "s_state"))
  # Calaculate installed capacity by standard.
  install_data <- install_data %>% 
    mutate(standard_capacity=Capacity-initial_cap)
  return(install_data)
}

size_grouping <- function(site_details){
  # Catergorise site by sample ac capacity.
  site_details <- site_details %>%
    mutate(Grouping=ifelse(first_ac>=30, "30-100kW" ,"<30 kW"))
  return(site_details)
}

site_catergorisation <- function(combined_data){
  # Processes installed month. Setting missing month values to jan 2005, and
  # using assumed day of month as the 28th. Then catergorising into stanard 
  # version based on date.
  combined_data <- combined_data %>%
    mutate(pv_installation_year_month=
             ifelse(pv_installation_year_month=="", "2015-11", 
                    pv_installation_year_month)) %>% 
    mutate(pv_installation_year_month=
             ifelse(pv_installation_year_month=="0/01/1900", "2015-11", 
                    pv_installation_year_month)) %>% 
    mutate(pv_installation_year_month=
             ifelse(is.na(pv_installation_year_month), 
                    "2015-11", pv_installation_year_month)) %>% 
    mutate(pv_installation_year_month=
             ifelse(nchar(pv_installation_year_month)==10,
                    pv_installation_year_month, 
                    paste0(pv_installation_year_month, "-28"))) %>% 
    mutate(pv_installation_year_month=
             ymd(pv_installation_year_month)) %>% 
    mutate(Standard_Version= 
             ifelse(pv_installation_year_month<"2015-10-01", "AS4777.3:2005", 
                    ifelse(pv_installation_year_month>="2016-11-01", 
                           "AS4777.2:2015", "Transition")))
  return(combined_data)
}

combine_data_tables <- function(time_series_data, circuit_details, 
                                site_details) {
  circuit_details <- process_raw_circuit_details(circuit_details)
  site_details <- site_catergorisation(site_details)
  site_details <- size_grouping(site_details)
  time_series_data <- inner_join(time_series_data, circuit_details, by="c_id")
  time_series_data <- inner_join(time_series_data, site_details, by="site_id")
  time_series_data <- perform_power_calculations(time_series_data)
  return(time_series_data)
}