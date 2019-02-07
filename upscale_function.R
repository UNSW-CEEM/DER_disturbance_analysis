upscale <- function(performace_data, install_capacity){
  performace_data_p <- group_by(performace_data, ts, site_id, clean)
  performace_data_a <- group_by(performace_data, site_id, clean)
  performace_data_p <- summarise(performace_data_p , power_kW=sum(power_kW))
  performace_data_a <- summarise(performace_data_a , sample_capacity=first(sum_ac), 
                                 s_state=first(s_state), 
                                 Standard_Version=first(Standard_Version), 
                                 Grouping=first(Grouping))
  performace_data_p <- as.data.frame(performace_data_p)
  performace_data_a <- as.data.frame(performace_data_a)
  performace_data <- left_join(performace_data_p, performace_data_a, on='site_id')
  performace_data <- group_by(performace_data, ts, s_state, Standard_Version, 
                              Grouping, clean)
  performace_data <- summarise(performace_data , power_kW=sum(power_kW),
                               sample_capacity=sum(sample_capacity))
  performace_data <- as.data.frame(performace_data)
  performace_data <- performace_data %>%
    mutate(date=as.Date(ts))
  install_capacity <- filter(install_capacity, date < min(performace_data$date))
  install_capacity <- group_by(install_capacity, Standard_Version, Grouping,  s_state)
  install_capacity <- summarise(install_capacity, standard_capacity=max(standard_capacity))
  install_capacity <- as.data.frame(install_capacity)
  performance_and_install <- inner_join(performace_data, install_capacity,
                                        by=c("Grouping", "Standard_Version", "s_state"))
  performance_and_install <- performance_and_install %>%
    mutate(power_kW=(power_kW/sample_capacity)*standard_capacity)
  return(performance_and_install)
}