upscale <- function(performance_data, install_capacity){
  performance_data <- performance_data %>% distinct(site_id, ts, clean, .keep_all=TRUE)
  performance_data <- group_by(performance_data, ts, s_state, Standard_Version, 
                              Grouping, clean)
  performance_data <- summarise(performance_data , 
                                performance_factor=mean(site_performance_factor))
  performance_data <- as.data.frame(performance_data)
  performance_data <- performance_data %>%
    mutate(date=as.Date(ts))
  install_capacity <- filter(install_capacity, date < min(performance_data$date))
  install_capacity <- group_by(install_capacity, Standard_Version, Grouping,  s_state)
  install_capacity <- summarise(install_capacity, standard_capacity=max(standard_capacity))
  install_capacity <- as.data.frame(install_capacity)
  performance_and_install <- inner_join(performance_data, install_capacity,
                                        by=c("Grouping", "Standard_Version", "s_state"))
  performance_and_install <- performance_and_install %>%
    mutate(power_kW=performance_factor * standard_capacity)
  performance_and_install <- performance_and_install[,
    c("ts", "s_state", "Standard_Version", "Grouping", "clean", 
      "performance_factor", "standard_capacity", "power_kW")]
  return(performance_and_install)
}

calc_site_performance_factors <- function(performance_data){
  assert_upscale_assumptions(performance_data)
  performace_data_p <- group_by(performance_data, ts, site_id, clean)
  performace_data_p <- summarise(performace_data_p , 
                                 site_performance_factor=sum(power_kW))
  performace_data_p <- as.data.frame(performace_data_p)
  performance_data <- left_join(performance_data, performace_data_p, 
                                on=c('site_id', 'ts', 'clean'))
  performance_data <- performance_data %>% 
    mutate(site_performance_factor=(site_performance_factor/sum_ac))
  return(performance_data)
}

assert_upscale_assumptions <- function(performance_data){
  # We assume all power data is numeric
  assert_that(all(is.numeric(performance_data$power_kW)) &
              all(!is.nan(performance_data$power_kW)), msg= "Not all power 
              data is numeric")
  # We assume all ac data is numeric
  assert_that(all(is.numeric(performance_data$sum_ac)) &
                all(!is.nan(performance_data$sum_ac)), msg= "Not all ac 
              data is numeric")
  # We assume all ac data is non zero
  assert_that(all(performance_data$sum_ac>0.01), msg= "Not all ac 
              data is greater than zero")
  # We assume no data duplication
  performance_data_temp <- performance_data[, c("ts", "c_id", "clean")]
  assert_that(all(!duplicated(performance_data_temp)), 
              msg="There are duplicate entries")
}