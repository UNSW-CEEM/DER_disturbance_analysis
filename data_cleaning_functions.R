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

get_post_code_lat_long <- function(postcode, postcode_data){
  postcode_data <- filter(postcode_data, ï..postcode==postcode)
  lat_and_long <- c(postcode_data$long[1], postcode_data$lat[1])
  return(lat_and_long)
}

calc_sunset_time <- function(date, lat, long){
  sunset <- getSunlightTimes(date, lat, 
                             lon=long, keep=c('sunset'))$sunset[1]
  return(sunset)
}

calc_sunrise_time <- function(date, lat, long){
  sunrise <- getSunlightTimes(date, lat=lat, 
                             lon=long, keep=c('sunrise'))$sunrise[1]
  return(sunrise)
}

clean_connection_types <- function(combined_data, circuit_details, postcode_data){
  calc_sunrise_time_v <- Vectorize(calc_sunrise_time)
  calc_sunset_time_v <- Vectorize(calc_sunset_time)
  t0 = Sys.time()
  postcode_data <- filter(postcode_data, !is.na(lat) & !is.na(long))
  print('1')
  #postcode_data$lat <-   gsub(':', ' ', postcode_data$lat)
  print('2')
  #postcode_data$long <-   gsub(':', ' ', postcode_data$long)
  print('3')
  #postcode_data$lat <- measurements::conv_unit(postcode_data$lat, from = 'deg_min_sec', to = 'dec_deg')
  postcode_data <- mutate(postcode_data, lat=as.numeric(lat))
  print('4')
  #postcode_data$lon <- measurements::conv_unit(postcode_data$long, from = 'deg_min_sec', to = 'dec_deg')
  postcode_data <- mutate(postcode_data, lon=as.numeric(long))
  postcode_data$date <- as.Date(combined_data$ts[1])
  postcode_data <- mutate(postcode_data, sunrise=getSunlightTimes(data=postcode_data, tz="Australia/Brisbane", keep=c('sunrise'))$sunrise)
  postcode_data <- mutate(postcode_data, sunset=getSunlightTimes(data=postcode_data, tz="Australia/Brisbane", keep=c('sunset'))$sunset)
  postcode_data <- mutate(postcode_data, sunrise=sunrise-60*60)
  postcode_data <- mutate(postcode_data, sunset=sunset+60*60)
  postcode_data <- mutate(postcode_data, dis_sunrise=as.character(sunrise))
  postcode_data <- mutate(postcode_data, dis_sunset=as.character(sunset))
  print('5')
  print(Sys.time()-t0)
  combined_data <- left_join(combined_data, select(postcode_data, postcode, sunrise, sunset, dis_sunrise, dis_sunset), by=c("s_postcode" = "postcode"))
  combined_data <- filter(combined_data,!is.na(ts))
  t0 = Sys.time()
  combined_data <- mutate(combined_data, daylight=ifelse(ts>sunrise & ts<sunset,1,0))
  print(Sys.time()-t0)
  combined_data <- group_by(combined_data, c_id)
  combined_data <- summarise(
    combined_data, energy_in_day_light_hours= sum(abs(e[daylight==1]))/1000/(60*60),
    energy_in_non_day_light_hours= sum(abs(e[daylight!=1]))/1000/(60*60),
    con_type=first(con_type), sunrise=first(dis_sunrise), 
    sunset=first(dis_sunset), first_ac=first(first_ac),
    min_power=min(power_kW), max_power=max(power_kW), polarity=first(polarity))
  combined_data <- as.data.frame(combined_data)
  combined_data <- mutate(combined_data, old_con_type=con_type)
  combined_data <- mutate(combined_data, old_polarity=polarity)
  combined_data <- mutate(
    combined_data, frac_in_day_light_hours=
      energy_in_day_light_hours/
      (energy_in_day_light_hours+energy_in_non_day_light_hours))
  combined_data <- mutate(
    combined_data, con_type=
      ifelse(frac_in_day_light_hours<0.75 & 
             con_type %in% c("pv_site_net", "pv_site", "pv_inverter_net") &
               (energy_in_day_light_hours+energy_in_non_day_light_hours) > first_ac * 24 * 0.01, 
             "load", con_type))
  combined_data <- mutate(combined_data, con_type=ifelse(max_power > first_ac * .1 & 
                                                min_power * -1 > first_ac * 0.1, "mixed", con_type))
  combined_data <- mutate(combined_data,
    polarity=ifelse(abs(min_power) > abs(max_power) & 
                    con_type %in% c("pv_site_net", "pv_site", "pv_inverter_net") &
                      (energy_in_day_light_hours+energy_in_non_day_light_hours) > first_ac * 24 * 0.01 &
                      min_power < 0.0, 
                    polarity * -1, polarity))
  combined_data <- mutate(combined_data, con_type_changed=ifelse(con_type!=old_con_type,1,0))
  combined_data <- mutate(combined_data, polarity_changed=ifelse(polarity!=old_polarity,1,0))
  circuit_details <- select(circuit_details, site_id, c_id, polarity, sa_ww_id)
  combined_data <- left_join(combined_data, circuit_details, on="c_id")
  return(combined_data)
}