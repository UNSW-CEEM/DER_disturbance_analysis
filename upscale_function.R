upscale <- function(performace_data, install_capacity){
  performace_data <- group_by(performace_data, ts, s_state, Standard_Version, 
                              Grouping)  
  performace_data <- summarise(performace_data , power_kW=sum(power_kW),
                               sample_capacity=sum(first_ac))
  performace_data <- as.data.frame(performace_data)
  performace_data <- performace_data %>%
    mutate(date=as.Date(ts))
  install_data <- filter(intsall_data, date < min(performace_data$date))
  install_data <- group_by(Standard_Version, Grouping,  s_state)
  install_data <- summarise(installed_start_standard, 
                                        initial_cap=max(standard_capacity))
  install_data <- as.data.frame(install_data)
  performance_and_install <- inner_join(performace_data, install_capacity, 
               by=c("Grouping", "Standard_Version", "s_state"))
  performance_and_install <- performance_and_install %>%
    mutate(power_kW=(power_kW/sample_capacity)*standard_capacity)
  return(performance_and_install)
}