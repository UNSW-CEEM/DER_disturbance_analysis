event_normalised_power <- function(combined_data, event_time){
  event_time_data <- filter(combined_data, ts == event_time)
  event_time_data <- select(event_time_data, power_kW, c_id, clean)
  event_time_data <- setnames(event_time_data, c("power_kW"), c("event_power_level"))
  combined_data <- left_join(combined_data, event_time_data, by=c("c_id", "clean"))
  combined_data <- mutate(combined_data, )
  combined_data <- mutate(combined_data, normalised_power_kW=ifelse(event_power_level>0.1,
                                                power_kW/event_power_level,
                                                NA))
  return(combined_data)
}