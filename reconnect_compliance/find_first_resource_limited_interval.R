find_first_resource_limited_interval <- function(ramp_rates, ramp_rate_change_threshold) {
  ramp_rates <- ramp_rates[order(ramp_rates$ts), ] %>%
    group_by(c_id) %>%
    mutate(ramp_rate_change = (ramp_rate - lag(ramp_rate))) %>%
    filter(ramp_rate_change < ramp_rate_change_threshold) %>%
    group_by(c_id) %>%
    summarise(resource_limited_interval = first(ts))
    select(c_id, resource_limited_interval)
  ramp_rates <- as.data.frame(ramp_rates)
  return(ramp_rates)
}
