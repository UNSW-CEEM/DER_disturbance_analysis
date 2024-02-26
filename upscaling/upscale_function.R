upscale <- function(performance_data, install_capacity) {
  performance_data <- performance_data %>%
    distinct(site_id, ts, clean, .keep_all = TRUE) %>%
    group_by(ts, s_state, Standard_Version, clean) %>%
    summarise(performance_factor = mean(site_performance_factor)) %>%
    as.data.frame() %>%
    mutate(date = as.Date(ts))
  install_capacity <- install_capacity[order(install_capacity$date),] %>%
    filter(date < min(performance_data$date)) %>%
    group_by(Standard_Version, Grouping, s_state) %>%
    summarise(standard_capacity = last(standard_capacity)) %>%
    as.data.frame()
  performance_and_install <- performance_data %>%
    inner_join(install_capacity, by = c("Standard_Version", "s_state")) %>%
    mutate(power_kW = performance_factor * standard_capacity)
  performance_and_install <- performance_and_install[, c(
    "ts",
    "s_state",
    "Standard_Version",
    "Grouping",
    "clean",
    "performance_factor",
    "standard_capacity",
    "power_kW"
  )]
  return(performance_and_install)
}

calc_site_performance_factors <- function(performance_data) {
  performace_data_p <- performance_data %>%
    group_by(ts, site_id, clean) %>%
    summarise(site_performance_factor = sum(power_kW)) %>%
    as.data.frame()
  performance_data <- performance_data %>%
    left_join(performace_data_p, by = c("site_id", "ts", "clean")) %>%
    mutate(site_performance_factor = (site_performance_factor / sum_ac))
  return(performance_data)
}
