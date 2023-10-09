calc_installed_capacity_by_standard_and_manufacturer <- function(install_data) {
  # Rename time column and categorise data based on inverter standards.
  install_data <- setnames(install_data, c("state", "date"), c("s_state", "pv_installation_year_month"))
  install_data <- site_categorisation(install_data)
  # Convert column names to same format as time solar analytics data.
  install_data <- setnames(install_data, c("pv_installation_year_month"), c("date"))
  # For each inverter standard group find the intall capacity when the standard
  # came into force.
  start_date = min(install_data$date)
  installed_start_standard <- group_by(install_data, Standard_Version, manufacturer, s_state)
  installed_start_standard <- summarise(
    installed_start_standard,
    initial_cap=get_initial_cap(install_data, min(date), first(s_state), first(manufacturer))
  )
  installed_start_standard <- as.data.frame(installed_start_standard)
  installed_start_standard <- mutate(
    installed_start_standard,
    initial_cap=ifelse(Standard_Version=="AS4777.3:2005", 0, initial_cap)
  )
  # Join the intial capacity to the cumulative capacity table.
  install_data <- inner_join(
    install_data,
    installed_start_standard,
    by=c("Standard_Version", "manufacturer", "s_state")
  )
  # Calculate installed capacity by standard.
  install_data <- mutate(install_data, standard_capacity=capacity-initial_cap)
  install_data <- select(
    install_data,
    date,
    s_state,
    manufacturer,
    capacity,
    number,
    Standard_Version,
    initial_cap,
    standard_capacity
  )
  return(install_data)
}

get_initial_cap <- function(install_data, min_date, state, man) {
  install_data <- filter(install_data, manufacturer == man)
  install_data <- filter(install_data, s_state == state)
  install_data <- filter(install_data, date < min_date)
  if (length(install_data$capacity) == 0) {
    initial_cap <- 0.0
  } else {
    initial_cap <-max(install_data$capacity)
  }
  return(initial_cap)
}
