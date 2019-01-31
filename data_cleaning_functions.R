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


ac_dc_ratio <- function(processed_site_details){
    processed_site_details <- processed_site_details %>% mutate(
      ac_dc_ratio = ac/(dc/1000)
    )
    return(processed_site_details)
  }

x

group_site_details_to_one_row_per_site <- function(site_details){
  site_details <- site_details %>% group_by(site_id)
  processed_site_details <- site_details %>%
    summarise(s_state=first(s_state), 
              pv_installation_year_month=first(pv_installation_year_month),
              sum_ac=sum(ac), sum_dc=first(dc), manufacture=paste(manufacture),
              model=paste(model), postcode=first(postcode))
  processed_site_details <- as.data.frame(processed_site_details)
}

check_for_ac_in_kw_and_dc_in_watts <- function(performace_data, processed_site_details){
  #Check the profile is not just niose, max output must be greater than 100W
}