categorise_response <- function(combined_data, event_time){
  event_window_data <- filter(combined_data, 
                              ts >= event_time & ts <= event_time + 60 * 3)
  event_window_data <- event_window_data[order(event_window_data$ts),]
  event_window_data <- group_by(event_window_data, c_id, clean)
  event_window_data <- summarise(event_window_data,
                                 d=first(d),
                                 event_power=first(power_kW),
                                 min_norm_power=min(power_kW/first(power_kW)),
                                 num_con_zeros=num_consecutive_zeros(power_kW),
                                 num_data_points=length(power_kW)-1)
  event_window_data <- mutate(
    event_window_data, 
    response_category=ifelse(event_power < 0.1, 'Off at t0', 'undefined'))
  
  event_window_data <- mutate(
    event_window_data, 
    response_category=ifelse(
      num_data_points < (60/d)*3 & response_category=='undefined', 
      'Not enough data', response_category))
  
  event_window_data <- mutate(
    event_window_data, 
    response_category=ifelse(
      response_category=='undefined' & min_norm_power >= 0.96, 
      'Ride Through', response_category))
  
  event_window_data <- mutate(
    event_window_data, 
    response_category=ifelse(
      response_category=='undefined' & min_norm_power < 0.96 & 
      num_con_zeros == 0, 'Curtail', response_category))
  
  event_window_data <- mutate(
    event_window_data, 
    response_category=ifelse(
      response_category=='undefined' & num_con_zeros == 1, 
      'Drop to Zero', response_category))
  
  event_window_data <- mutate(
    event_window_data, 
    response_category=ifelse(
      response_category=='undefined' & num_con_zeros > 1, 
      'Disconnect', response_category))
  
  event_window_data <- select(event_window_data, c_id, clean, response_category)
  combined_data <- left_join(combined_data, event_window_data, on=c("c_id", "clean"))
  return(combined_data)
}

num_consecutive_zeros <- function(event_power_vector){
  num_con_zeros <- 0
  if (length(event_power_vector) > 1){
    for (i in 2:length(event_power_vector)){
      if (num_con_zeros == 0){
        if(event_power_vector[i] < 0.1){
          num_con_zeros <- num_con_zeros + 1
        }
      } else {
        if(event_power_vector[i] < 0.1 & event_power_vector[i - 1] < 0.1){
          num_con_zeros <- num_con_zeros + 1
        }
      }
    }
  }
  return(num_con_zeros)
} 