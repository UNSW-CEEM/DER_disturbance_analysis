check_ac_capacity_using_peak_power <- 
  function(performace_data, processed_site_details){
    performace_data <- group_by(performace_data, site_id)  
    site_max_power <- summarise(performace_data , max_power_kW=max(power_kW),
                                 sum_ac=first(sum_ac))
    site_max_power <- as.data.frame(site_max_power)
    flagged_sites <- site_max_power %>% mutate(
      high_pf_flag = ifelse(max_power_kW > 1.5 * sum_ac, 1, 0)
    )
    flagged_sites <- flagged_sites[,c("site_id", "high_pf_flag")] 
    processed_site_details = left_join(processed_site_details, flagged_sites,
                                       on=c("site_id"))
    return(processed_site_details)
  }

calc_max_kw_per_site <- function(performance_data){
  performance_data <- group_by(performance_data, ts, site_id)  
  performance_data <- summarise(performance_data , power_kW=sum(power_kW))
  performance_data <- as.data.frame(performance_data)
  performance_data <- group_by(performance_data, site_id)  
  site_max_power <- summarise(performance_data , max_power_kW=max(power_kW))
  site_max_power <- as.data.frame(site_max_power)
  site_max_power <- site_max_power[,c("site_id", "max_power_kW")]
  return(site_max_power)
}

group_site_details_to_one_row_per_site <- function(site_details){
  site_details <- site_details %>% group_by(site_id)
  processed_site_details <- site_details %>%
    summarise(s_state=first(s_state), 
              pv_installation_year_month=first(pv_installation_year_month),
              sum_ac=sum(ac), sum_dc=first(dc),
              sum_ac_old=sum(ac_old), sum_dc_old=first(dc_old),
              ac_dc_ratio=mean(ac_dc_ratio),
              manufacturer=paste(manufacturer, collapse=' '),
              model=paste(model, collapse=' '), s_postcode=first(s_postcode))
  processed_site_details <- as.data.frame(processed_site_details)
  return(processed_site_details)
}

check_for_ac_in_kw_and_dc_in_watts <- function(performace_data, processed_site_details){
  #Check the profile is not just niose, max output must be greater than 100W
}

site_details_data_cleaning <- function(performance_data, site_details){
  #Ac in watts, Dc in watts
  #AC in watts, DC in watts, and AC size of just one inverter of many
  #AC in watts, DC in watts, but AC size of just one inverter of many and dc size of just once array
  #AC in watts of one inverter and dc of one panel
  site_details <- mutate(site_details, ac_old=ac)
  site_details <- mutate(site_details, dc_old=dc)
  site_details <- mutate(site_details, ac_dc_ratio=ac/dc)
  site_details <- mutate(site_details, sum_ac=ifelse(ac_dc_ratio > 0.1 & ac > 150, ac/1000, ac))
  
  site_details <- group_site_details_to_one_row_per_site(site_details)
  max_site_power <- calc_max_kw_per_site(performance_data)
  site_details <- left_join(site_details, max_site_power, on=c("site_id"))
  
  #AC in kW, DC in watts, but AC size of just one inverter of many
  #AC in kW, DC in watts, but AC size of just one inverter of many and dc size of just once array
  #AC in kW, of one inverter and dc of one panel
  site_details <- mutate(site_details, 
                         sum_dc=ifelse((sum_dc/1000)/abs(max_power_kW) < 0.9, 
                                       ceiling(max_power_kW/(sum_dc/1000)) * sum_dc, sum_dc))
  site_details <- mutate(site_details, change_dc=ifelse(sum_dc!=sum_dc_old,1,0))
  site_details <- mutate(site_details, 
                         sum_ac=ifelse(sum_ac/abs(max_power_kW) < 0.6, 
                                       ifelse(change_dc==0,
                                              ceiling((sum_dc/1000)/sum_ac) * sum_ac,
                                              ceiling(max_power_kW/sum_ac) * sum_ac), 
                                       sum_ac))
  site_details <- mutate(site_details, change_ac=ifelse(sum_ac!=sum_ac_old,1,0))

  return(site_details)
}

clean_connection_types <- function(combined_data, circuit_details, postcode_data){
  # Don't peform sunrise sunset based check if we do not have the postcode 
  # lat and longditude.
  postcode_data <- filter(postcode_data, !is.na(lat) & !is.na(long))
  # Make sure lat and longditude format is numeric, also need longditude labeled
  # lon.
  postcode_data <- mutate(postcode_data, lat=as.numeric(lat))
  postcode_data <- mutate(postcode_data, lon=as.numeric(long))
  # Need date of even to calaculate sunris and sunset times.
  postcode_data$date <- as.Date(combined_data$ts[1])
  # Find sunrise and sunset times on a postcode basis.
  postcode_data <- mutate(
    postcode_data, 
    sunrise=getSunlightTimes(data=postcode_data, tz="Australia/Brisbane", 
                             keep=c('sunrise'))$sunrise)
  postcode_data <- mutate(
    postcode_data, 
    sunset=getSunlightTimes(data=postcode_data, tz="Australia/Brisbane",
                            keep=c('sunset'))$sunset)
  # Create 1 hour buffer either side of sunrise and sunset to allow for large 
  # postcodes, as lat and lon is the postcode centre.
  postcode_data <- mutate(postcode_data, sunrise=sunrise-60*60)
  postcode_data <- mutate(postcode_data, sunset=sunset+60*60)
  # Format sunrise and sunset as character so it is displayed in brisbane time
  # in gui.
  postcode_data <- mutate(postcode_data, dis_sunrise=as.character(sunrise))
  postcode_data <- mutate(postcode_data, dis_sunset=as.character(sunset))
  # Merge sunrise and sunset times onto the timeseries data frame.
  combined_data <- left_join(combined_data, 
                             select(postcode_data, postcode, 
                                    sunrise, sunset, dis_sunrise, 
                                    dis_sunset), 
                             by=c("s_postcode" = "postcode"))
  # Determine if a data point is during daylight hours or not.
  combined_data <- mutate(combined_data, 
                          daylight=ifelse(ts>sunrise & ts<sunset,1,0))
  # Group data by c_id, calculating values needed to perform data cleaning
  combined_data <- group_by(combined_data, c_id)
  combined_data <- summarise(
    combined_data, 
    energy_in_day_light_hours=sum(abs(e[daylight==1]))/1000/(60*60),
    energy_in_non_day_light_hours= sum(abs(e[daylight!=1]))/1000/(60*60),
    con_type=first(con_type), sunrise=first(dis_sunrise), 
    sunset=first(dis_sunset), first_ac=first(first_ac),
    min_power=min(power_kW), max_power=max(power_kW), polarity=first(polarity))
  combined_data <- as.data.frame(combined_data)
  # Record details before changes
  combined_data <- mutate(combined_data, old_con_type=con_type)
  combined_data <- mutate(combined_data, old_polarity=polarity)
  # Calculate the fraction of gen/load that occured during daylight hours, just
  # use absolute value of power as polarity has not been checked yet.
  combined_data <- mutate(
    combined_data, 
    frac_in_day_light_hours=energy_in_day_light_hours/
      (energy_in_day_light_hours+energy_in_non_day_light_hours))
  # Check for pv connection type, if the type is pv but more than 25% of the 
  # gen/load was outside daylight hours then change to type 'load'. Only change
  # if system has operated at above an avergae of 1% capacity
  combined_data <- mutate(
    combined_data, 
    con_type=ifelse(frac_in_day_light_hours<0.75 & 
      con_type %in% c("pv_site_net", "pv_site", "pv_inverter_net") &
      (energy_in_day_light_hours+energy_in_non_day_light_hours) > first_ac * 24 * 0.01, 
      "load", con_type))
  # Check for power flowing in both negative and positive direction, if this 
  # occurs with values large than 10 % of the inverter capacity then change 
  # connection type to 'mixed'
  combined_data <- mutate(combined_data, 
                          con_type=ifelse(max_power > first_ac * .1 & 
                          min_power * -1 > first_ac * 0.1, "mixed", con_type))
  # Check for peak power occuring as a negative value i.e. reversed polarity,
  # if this occurs for pv systems that have operated at above an avergae of 1%
  # capacity then swap polarity.
  combined_data <- mutate(combined_data,
    polarity=ifelse(abs(min_power) > abs(max_power) & 
                    con_type %in% c("pv_site_net", "pv_site", "pv_inverter_net") &
                    (energy_in_day_light_hours+energy_in_non_day_light_hours) > first_ac * 24 * 0.01 &
                    min_power < 0.0, 
                    polarity * -1, polarity))
  # Flag systems with changed connection type or polarity.
  combined_data <- mutate(combined_data, con_type_changed=ifelse(con_type!=old_con_type,1,0))
  combined_data <- mutate(combined_data, polarity_changed=ifelse(polarity!=old_polarity,1,0))
  # Select the values from the orginal circuit details that would not be changed 
  # by cleaning, then merge back in with details updated or created by cleaning
  circuit_details <- select(circuit_details, site_id, c_id, sa_ww_id)
  combined_data <- left_join(combined_data, circuit_details, on="c_id")
  return(combined_data)
}