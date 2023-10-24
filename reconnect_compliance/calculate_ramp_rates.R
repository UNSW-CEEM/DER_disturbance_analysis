calculate_ramp_rates <- function(normalised_power_profiles) {
  normalised_power_profiles <- normalised_power_profiles[order(normalised_power_profiles$ts),]
  normalised_power_profiles <- normalised_power_profiles %>%
    group_by(c_id) %>%
    mutate(
      ramp_rate = (
        c_id_daily_norm_power - lag(c_id_daily_norm_power)
      ) / as.numeric(difftime(ts, lag(ts), units = "mins"))
    )
  normalised_power_profiles <- select(normalised_power_profiles, ts, c_id, ramp_rate)
  normalised_power_profiles <- as.data.frame(normalised_power_profiles)
  return(normalised_power_profiles)
}
