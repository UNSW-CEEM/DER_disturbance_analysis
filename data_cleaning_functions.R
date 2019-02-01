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
  performance_data <- summarise(performance_data , power_kW=sum(power_kW), n_circuits=length(unique(c_id)))
  performance_data <- as.data.frame(performance_data)
  performance_data <- group_by(performance_data, site_id)  
  site_max_power <- summarise(performance_data , max_power_kW=max(power_kW),
                              n_circuits=max(n_circuits))
  site_max_power <- as.data.frame(site_max_power)
  site_max_power <- site_max_power[,c("site_id", "max_power_kW", "n_circuits")]
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
              n_site_ids=length(ac),
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