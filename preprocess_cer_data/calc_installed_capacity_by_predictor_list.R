calc_installed_capacity_by_all_predictors <- function(install_data){
  # Rename time column and categorise data based on inverter standards.
  install_data <- setnames(install_data, c("state", "date", "postcode", "sizegroup"), 
                           c("s_state", "pv_installation_year_month", "s_postcode", "Grouping"))
  install_data <- site_categorisation(install_data)
  # Convert column names to same format as time solar analytics data.
  install_data <- setnames(install_data, c("pv_installation_year_month"), c("date"))
  # For each inverter standard group find the install capacity when the standard
  # came into force.
  start_date = min(install_data$date)
  installed_start_standard <- group_by(install_data, Standard_Version, manufacturer, s_postcode, Grouping, s_state)
  installed_start_standard <- summarise(installed_start_standard, 
                                        initial_cap=get_initial_cap_all_predictors(install_data, min(date), first(s_state),
                                                                    first(manufacturer), first(s_postcode), 
                                                                    first(Grouping)))
  installed_start_standard <- as.data.frame(installed_start_standard)
  installed_start_standard <- mutate(installed_start_standard, 
                                     initial_cap=ifelse(Standard_Version=="AS4777.3:2005",0,initial_cap))
  # Join the initial capacity to the cumulative capacity table.
  install_data <- inner_join(install_data, installed_start_standard, by=c("Standard_Version", "manufacturer", 
                                                                          "s_postcode", "Grouping", "s_state"))
  # Calculate installed capacity by standard.
  install_data <- mutate(install_data, standard_capacity=capacity-initial_cap)
  install_data <- select(install_data, date, s_state, s_postcode, manufacturer, Grouping, capacity, number, 
                         Standard_Version, initial_cap, standard_capacity)
  return(install_data)
}

get_initial_cap_all_predictors <- function(install_data, min_date, state, man, postcode, size_grp){
  install_data <- filter(install_data, manufacturer == man)
  install_data <- filter(install_data, s_state == state)
  install_data <- filter(install_data, s_postcode == postcode)
  install_data <- filter(install_data, Grouping == size_grp)
  install_data <- filter(install_data, date < min_date)
  if (length(install_data$capacity) == 0){
    initial_cap <- 0.0
  } else {
    initial_cap <-max(install_data$capacity)
  }
  return(initial_cap)
}