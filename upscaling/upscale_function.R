upscale <- function(performance_data, install_capacity){
  performance_data <- distinct(performance_data, site_id, ts, clean, .keep_all=TRUE)
  performance_data <- group_by(performance_data, ts, s_state, Standard_Version, clean)
  performance_data <- summarise(performance_data , performance_factor=mean(site_performance_factor))
  performance_data <- as.data.frame(performance_data)
  performance_data <- mutate(performance_data, date=as.Date(ts))
  install_capacity <- install_capacity[order(install_capacity$date), ]
  install_capacity <- filter(install_capacity, date < min(performance_data$date))
  install_capacity <- group_by(install_capacity, Standard_Version, Grouping,  s_state)
  install_capacity <- summarise(install_capacity, standard_capacity=last(standard_capacity))
  install_capacity <- as.data.frame(install_capacity)
  performance_and_install <- inner_join(performance_data, install_capacity, 
                                        by=c("Standard_Version", "s_state"))
  performance_and_install <- mutate(performance_and_install, power_kW=performance_factor * standard_capacity)
  performance_and_install <- performance_and_install[,c("ts", "s_state", "Standard_Version", "Grouping", "clean", 
                                                        "performance_factor", "standard_capacity", "power_kW")]
  return(performance_and_install)
}

calc_site_performance_factors <- function(performance_data){
  performace_data_p <- group_by(performance_data, ts, site_id, clean)
  performace_data_p <- summarise(performace_data_p , site_performance_factor=sum(power_kW))
  performace_data_p <- as.data.frame(performace_data_p)
  performance_data <- left_join(performance_data, performace_data_p, on=c('site_id', 'ts', 'clean'))
  performance_data <- mutate(performance_data, site_performance_factor=(site_performance_factor/sum_ac))
  return(performance_data)
}
