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


ac_dc_ratio <- 
  function(processed_site_details){
    processed_site_details <- processed_site_details %>% mutate(
      ac_dc_ratio = ac/(dc/1000)
    )
    return(processed_site_details)
  }