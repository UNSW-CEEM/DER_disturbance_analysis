
process_raw_time_series_data <- function(time_series_data){
  # Sometimes data from solar analytics contains rows that are additional 
  # headers explicity remove these.
  gc()
  time_series_data <- time_series_data[!time_series_data$c_id=="c_id",]
  # Convert data that comes as strings to numeric where applicable.
  #time_series_data <- time_series_data %>%  mutate(d = as.numeric(d))
  # Convert time stamp to date time object assuming UTC time is being used.
  time_series_data <- time_series_data %>%  mutate(ts = fastPOSIXct(ts))
  time_series_data_1 <- group_by(time_series_data, c_id) %>% summarise(Count=n()) %>% filter(Count==1)
  time_series_data <- anti_join(time_series_data, time_series_data_1,by="c_id")
  time_series_data <- mutate(time_series_data, d=as.numeric(d))
  
  #time_series_data <- mutate(time_series_data, d=ifelse(d == 0, 5, d))
  #time_series_durations = group_by(time_series_data, c_id)
  #time_series_durations <- summarise(time_series_durations, d_filter=duration_mode(ts), d_min=duration_min(ts))
  gc()
  #time_series_durations <- filter(time_series_durations, d_filter==d_min)
  #time_series_data <- left_join(time_series_data, time_series_durations, by="c_id")
  # Assert assumptions about data set
  
  time_series_data <- time_series_data %>% group_by(c_id) %>% mutate(interval = ts - lag(ts, order_by = ts))
  gc()
  time_series_data <- mutate(time_series_data, d=ifelse(interval==5,5,d))
  gc()
  time_series_data <- select(time_series_data, ts, c_id, e, f, v, d) 
  gc()
  assert_raw_time_series_assumptions(time_series_data)
  # Change time zone to NEM standard time.
  time_series_data <- mutate(time_series_data, ts = with_tz(ts,"Australia/Brisbane"))
  # Covert columns to numeric type.
  time_series_data <- mutate(time_series_data, e = as.numeric(e))
  time_series_data <- mutate(time_series_data, v = as.numeric(v))
  processed_time_series_data <- mutate(time_series_data, f = as.numeric(f))
  return(processed_time_series_data)
}

assert_raw_time_series_assumptions <- function(raw_time_series_data){
  # Check in coming timeseries data for conformance to data processing 
  # assumptions
  # We assume that the only reason for a character value to appear in a numeric
  # column is if we have header duplication.
  c_ids <- as.numeric(raw_time_series_data$c_id)
  assert_that(all(c_ids == floor(c_ids)), msg="Not all circuit IDs are integers")
  # We assume that all values in the ts column were of the format year month day hour minute second
  assert_that(all(!is.na(raw_time_series_data$ts)), msg="Not all ts datetimes in correct format")
  # We assume that all values in the "e" column can be safely converted to numeric type
  assert_that(all(!is.na(as.numeric(raw_time_series_data$e))), msg="Not all e values could be interprested as numbers")
  # We assume after interpreting NAs as 5 s data that all duration data should equal 60, 30 or 5
  #assert_that(all(raw_time_series_data$d==5 | raw_time_series_data$d==30 | raw_time_series_data$d==60), 
  #              msg="Not all duration values are 5, 30 or 60")
}

get_time_offsets <- function(time_series_data){
  time_series_data <- mutate(time_series_data, time_offset=format(ts, "%S"))
  #offsets <- group_by(offsets, c_id, time_offset)
  #offsets <- summarise(offsets, time_offset_count=length(time_offset))
  #offsets <- filter(offsets, time_offset_count>=100)
  #offsets <- group_by(offsets, c_id)
  #offsets <- summarise(offsets, time_offset=min(as.numeric(time_offset)))
  #time_series_data <- left_join(time_series_data, offsets, by='c_id')
  return(time_series_data)
}

get_time_series_unique_offsets <- function(time_series_data){
  unique_offsets <- unique(time_series_data$time_offset)
  return(unique_offsets)
}

make_offset_filter_label <- function(sample_counts, unique_offsets){
  label <- "Select time offset data subset to use, ("
  for(i in 1:length(unique_offsets)){
    label <- paste(label, unique_offsets[i], ": n=", sample_counts[i], ", ",sep='')
  }
  label <- paste(label, ")",sep='')
  return(label)
}

get_offset_sample_counts <- function(time_series_data, unique_offsets){
  time_series_data <- distinct(time_series_data, c_id, .keep_all=TRUE)
  sample_counts <- c()
  for(i in 1:length(unique_offsets)){
    sample_counts <- c(sample_counts, length(filter(time_series_data, time_offset==unique_offsets[i])$c_id))
  }
  return(sample_counts)
}

get_duration_sample_counts <- function(time_series_data, duration_options){
  time_series_data <- distinct(time_series_data, c_id, .keep_all=TRUE)
  sample_counts <- c()
  for(i in 1:length(duration_options)){
    sample_counts <- c(sample_counts, length(filter(time_series_data, d==as.numeric(duration_options[i]))$c_id))
  }
  return(sample_counts)
}

process_raw_circuit_details <- function(circuit_details){
  assert_circuit_details_assumptions(circuit_details)
  # Filter circuit id by connection type, just including solar data.
  site_types <- c("pv_site_net", "pv_site", "pv_inverter_net", "pv_inverter")
  processed_circuit_details <- filter(circuit_details, con_type %in% site_types)
  processed_circuit_details <- mutate(processed_circuit_details, c_id=as.character(c_id))
  return(processed_circuit_details)
}

assert_circuit_details_assumptions <- function(data){
  # polarity needs to be type numeric, either postive of negative one
  assert_that(all(data$polarity == 1 | data$polarity == -1), msg="Not all polarity values are 1 or -1")
}

process_raw_site_details <- function(site_details){
  site_details <- filter(site_details, !is.na(ac) & ac != "")
  assert_raw_site_details_assumptions(site_details)
  # The data can contain duplicate site ids, these need to be sumarised so there
  # is one row per site id. AC power is summed so sites with more than 100kW 
  # can be filtered out of the data set. The first ac value is taken as a sample
  # of the site, it is not summed, as latter when the data is joined to the 
  # circuit data (which may have mutiple rows be site_id) this would create
  # apparent additional ac capacity.
  site_details <- group_by(site_details, site_id)
  processed_site_details <- summarise(site_details, s_state=first(s_state), 
                                      pv_installation_year_month=first(pv_installation_year_month),
                                      sum_ac=sum(ac), first_ac=first(ac), s_postcode=first(s_postcode),
                                      manufacturer=paste(manufacturer, collapse=' '),
                                      model=paste(model, collapse=' '))
  processed_site_details <- as.data.frame(processed_site_details)
  return(processed_site_details)}

assert_raw_site_details_assumptions <- function(site_details){
  # Check in coming site data for conformance to data processing assumptions
  # We assume that only possible s_state values are NSW, QLD, VIC, TAS, SA, WA, NT, ACT
  s_state <- site_details$s_state
  assert_that(all(s_state == "NSW" | s_state == "QLD" | s_state == "VIC" | s_state == "TAS" | s_state == "SA" |
                    s_state == "WA" | s_state == "ACT"), msg="State values outside expected set NSW, ACT, SA etc")
  # We assume that for each site id there is only one distinct s_state value  and s_postcode value
  site_details_grouped <- group_by(site_details, site_id)
  site_details_grouped <- summarise(site_details_grouped, s_state=unique(s_state), s_postcode=unique(s_postcode))
  site_details_grouped <- as.data.frame(site_details_grouped)
  assert_that(all(lapply(site_details_grouped$s_state, length)==1), 
              msg="Some stites have mutiple distinct s_state values")
  assert_that(all(lapply(site_details_grouped$s_postcode, length)==1),
              msg="Some stites have mutiple distinct s_postcode values")
  # We assume ac and dc values can be converted to numeric without be turned
  # into NAs
  assert_that(all(!is.na(as.numeric(site_details$ac))))
  #assert_that(all(!is.na(as.numeric(site_details$dc))))
}

perform_power_calculations <- function(master_data_table){
  # Calculate the average power output over the sample time base on the 
  # cumulative energy and duration length.Assuming energy is joules and duration is in seconds.
  master_data_table <- mutate(master_data_table, e_polarity=e*polarity)
  master_data_table <- mutate(master_data_table, power_kW = e_polarity/(d * 1000))
  return(master_data_table)
}

process_install_data <- function(install_data){
  assert_install_data_assumptions(install_data)
  # Rename time column and catergorise data based on inverter standards.
  install_data <- mutate(install_data, pv_installation_year_month=index)
  install_data <- site_categorisation(install_data)
  # Convert column names to same format as time solar analytics data.
  install_data <- setnames(install_data, c("pv_installation_year_month", "State"), c("date", "s_state"))
  # For each inverter standard group find the intall capacity when the standard
  # came into force.
  start_date =min(install_data$date)
  installed_start_standard <- group_by(install_data, Standard_Version, Grouping, s_state)
  installed_start_standard <- summarise(
    installed_start_standard, 
    initial_cap=
      install_data$Capacity[max(c(install_data$date[(install_data$Capacity < min(Capacity)) &
                                                  (install_data$s_state == first(s_state)) & 
                                                  (install_data$Grouping == first(Grouping))],start_date)) == install_data$date & 
                              install_data$s_state == first(s_state) & 
                              install_data$Grouping == first(Grouping)])
  installed_start_standard <- as.data.frame(installed_start_standard)
  installed_start_standard <- mutate(installed_start_standard, 
                                     initial_cap=ifelse(Standard_Version=="AS4777.3:2005",0,initial_cap))
  # Join the intial capacity to the cumulative capacity table.
  install_data <- inner_join(install_data, installed_start_standard, by=c("Standard_Version", "Grouping", "s_state"))
  # Calaculate installed capacity by standard.
  install_data <- mutate(install_data, standard_capacity=Capacity-initial_cap)
  return(install_data)
}

assert_install_data_assumptions <- function(install_data){
  # Assert that the date column is convertable to a date object using the assumed format.
  assert_that(all(!is.na(as.Date(install_data$index, format="%Y-%m-%d"))), 
              msg="pv_installation_year_month has an unexpected format, should be YYYY-MM-DD")
  # Assert groupings only one of two options 
  assert_that(all(install_data$Grouping %in% c("30-100kW" ,"<30 kW")), msg="Grouping values in install data do not match
              the expected values")
  # Assert that all capacity values can be converted to numeric without creating nas. 
  assert_that(all(!is.na(as.numeric(install_data$Capacity))), msg="Not all capacity values can convert to numeric in 
              install data")
  # Assert that State values are within expected set.
  assert_that(all(install_data$State %in% c("NSW", "VIC", "SA", "TAS", "QLD", "NT", "ACT", "WA")), 
              msg="State values in install data do not match the expected values")
  # Assert that the first date in the install data is before the start of the transition peroid 
  assert_that(min(ymd(install_data$index))<ymd("2015-10-01"), msg="Install data first entery does not predate start of
              transition peroid")
  
}

size_grouping <- function(site_details){
  # Catergorise site by sample ac capacity.
  site_details <- mutate(site_details, Grouping=ifelse(first_ac>=30, "30-100kW" ,"<30 kW"))
  return(site_details)
}

process_postcode_data <-function(postcode_data){
  assert_postcode_data_assumptions(postcode_data)
  postcode_data <- mutate(postcode_data, postcode = as.integer(postcode))
  postcode_data <- filter(postcode_data, !is.na(lat) & !is.na(lon))
  return(postcode_data)
}

assert_postcode_data_assumptions <- function(postcode_data){
  # We assume that all values in the lat column can be safely converted to numeric type
  assert_that(all(!is.na(as.numeric(postcode_data$lat))), msg="Not all lat values could be interprested as numbers")
  # We assume that all values in the lon column can be safely converted to numeric type
  assert_that(all(!is.na(as.numeric(postcode_data$lon))), msg="Not all lat values could be interprested as numbers")
  # We assume that all values in the s_postcode column can be safely converted to numeric type
  assert_that(all(!is.na(as.numeric(postcode_data$postcode))), msg="Not all s_postcode values could be interprested as numbers")
}

site_categorisation <- function(combined_data){
  # Processes installed month. Setting missing month values to jan 2005, and
  # using assumed day of month as the 28th. Then catergorising into stanard 
  # version based on date.
  combined_data <- combined_data %>%
    mutate(pv_installation_year_month=
             ifelse(pv_installation_year_month=="", "2015-11", 
                    pv_installation_year_month)) %>%
    mutate(pv_installation_year_month=
             ifelse(pv_installation_year_month=="NaT", "2015-11", 
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

assert_site_install_date_assumptions <- function(site_details){
  # Check in coming  data for conformance to data processing assumptions
  # We assume that the pv installation year month will be either of the form
  # YYYY-MM or YYYY-MM-DD
  assert_that(all(length(pv_installation_year_month) %in% c(7, 10)), 
              msg="pv_installation_year_month has an unexpected format, should
              be YYYY-MM or YYYY-MM-DD")
  assert_that(all(length(pv_installation_year_month) %in% c(7, 10)), 
              msg="pv_installation_year_month has an unexpected format, should
              be YYYY-MM or YYYY-MM-DD")
}

combine_data_tables <- function(time_series_data, circuit_details, 
                                site_details) {
  circuit_details <- process_raw_circuit_details(circuit_details)
  site_details <- site_categorisation(site_details)
  site_details <- size_grouping(site_details)
  time_series_data <- inner_join(time_series_data, circuit_details, by="c_id")
  time_series_data <- inner_join(time_series_data, site_details, by="site_id")
  time_series_data <- perform_power_calculations(time_series_data)
  return(time_series_data)
}

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

duration_mode <- function(time_vector){
  ds <- c()
  time_vector <- sort(time_vector)
  ds <- as.numeric(diff(time_vector), units='secs')
  mode_ds <- get_mode(ds)
  return(mode_ds)
}

duration_mean <- function(time_vector){
  ds <- c()
  time_vector <- sort(time_vector)
  ds <- as.numeric(diff(time_vector), units='secs')
  mean_ds <- mean(ds)
  return(mean_ds)
}

duration_min <- function(time_vector){
  ds <- c()
  time_vector <- sort(time_vector)
  ds <- as.numeric(diff(time_vector), units='secs')
  min_ds <- min(ds)
  return(min_ds)
}

calc_interval <- function(time_series){
  time_series <- time_series[order(time_series$ts),]
  time_series <- time_series %>% group_by(c_id) %>% mutate(interval = ts - lag(ts))
  return(time_series)
}

write_sql_filter <- function(c_ids){
  filter_statement_head <- "SELECT * FROM file JOIN TABLE(SELECT column_value FROM sys.dbms_debug_vc2coll("
  
  filter_statement_c_id_list <- paste(c_ids, collapse = ', ')
  
  filter_statement_tail <- ")) c on table.c_id = c.column_value;"
  
  filter_statement <- paste0(c(filter_statement_head, filter_statement_c_id_list, filter_statement_tail), 
                             collapse = "")
  return(filter_statement)
}