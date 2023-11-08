calc_installed_capacity_by_standard_and_manufacturer <- function(install_data) {
  # Rename time column and categorise data based on inverter standards.
  install_data <- setnames(install_data, c("state", "date"), c("s_state", "pv_installation_year_month"))
  install_data <- site_categorisation(install_data)
  # Convert column names to same format as time Solar Analytics data.
  install_data <- setnames(install_data, c("pv_installation_year_month"), c("date"))
  # For each inverter standard group find the install capacity when the standard came into force.
  start_date = min(install_data$date)
  installed_start_standard <- install_data %>%
    group_by(Standard_Version, manufacturer, s_state) %>%
    summarise(initial_cap = get_initial_cap(install_data, min(date), first(s_state), first(manufacturer))) %>%
    as.data.frame() %>%
    mutate(initial_cap = ifelse(Standard_Version == "AS4777.3:2005", 0, initial_cap))
  # Join the initial capacity to the cumulative capacity table.
  install_data <- install_data %>%
    inner_join(installed_start_standard, by = c("Standard_Version", "manufacturer", "s_state")) %>%
    # Calculate installed capacity by standard.
    mutate(standard_capacity = capacity - initial_cap) %>%
    select(date, s_state, manufacturer, capacity, number, Standard_Version, initial_cap, standard_capacity)
  return(install_data)
}

get_initial_cap <- function(install_data, min_date, state, man) {
  install_data <- install_data %>%
    filter(manufacturer == man) %>%
    filter(s_state == state) %>%
    filter(date < min_date)
  if (length(install_data$capacity) == 0) {
    initial_cap <- 0.0
  } else {
    initial_cap <- max(install_data$capacity)
  }
  return(initial_cap)
}
