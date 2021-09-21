# if first ts != event_time -> NA (also lets count how many lose to this)

categorise_response <- function(combined_data, event_time, window_length,NED_threshold_pct = 0.8){
  event_window_data <- filter(combined_data, ts >= event_time & ts <= event_time + 60 * window_length)
  event_window_data <- event_window_data[order(event_window_data$ts),]
  event_window_data <- group_by(event_window_data, c_id, clean)
  event_window_data <- summarise(event_window_data, d=first(d), event_power=first(power_kW),
                                 min_norm_power=min(power_kW/first(power_kW)),
                                 num_con_zeros=num_consecutive_zeros(power_kW),
                                 num_data_points=length(power_kW)-1,
                                 first_ts = ifelse(first(ts)==event_time,"sampled","not sampled"))
  event_window_data <- categorise_by_response(event_window_data, window_length,NED_threshold_pct)
  event_window_data <- select(event_window_data, "c_id", "clean", "response_category")
  combined_data <- left_join(combined_data, event_window_data, on=c("c_id", "clean"))
  return(combined_data)
}

categorise_by_response <- function(event_window_data, window_length,NED_threshold_pct){
  event_window_data <- mutate(event_window_data, response_category=ifelse(
    event_power < 0.1, '5 Off at t0', 'Undefined'))
  
  # NED if sampled seconds below threshold
  event_window_data <- mutate(event_window_data, 
                              response_category=ifelse(num_data_points < (60/d)*window_length*NED_threshold_pct,
                                                       '6 Not enough data', response_category))
  # NED if first_ts not sampled 
  event_window_data <- mutate(event_window_data,
                              response_category=ifelse(first_ts=="not sampled",
                                                       '6 Not enough data', response_category))
  
  #
  event_window_data <- mutate(event_window_data, 
                              response_category=ifelse(response_category=='Undefined' & min_norm_power >= 0.96, 
                                                       '1 Ride Through', response_category))
  
  event_window_data <- mutate( event_window_data, response_category=ifelse(
    response_category=='Undefined' & min_norm_power < 0.96 & num_con_zeros == 0, '2 Curtail', response_category))
  
  event_window_data <- mutate(event_window_data, response_category=ifelse(
    response_category=='Undefined' & num_con_zeros == 1, '3 Drop to Zero', response_category))
  
  event_window_data <- mutate(event_window_data, response_category=ifelse(
    response_category=='Undefined' & num_con_zeros > 1, '4 Disconnect', response_category))
  
  return(event_window_data)
}

num_consecutive_zeros <- function(event_power_vector){
  num_con_zeros <- 0
  pt0 <- event_power_vector[1]
  if (length(event_power_vector) > 1 & pt0 > 0.1){
    for (i in 2:length(event_power_vector)){
      if (num_con_zeros == 0){
        if((event_power_vector[i]/pt0) < 0.05){
          num_con_zeros <- num_con_zeros + 1
        }
      } else {
        if(((event_power_vector[i]/pt0) < 0.05) & ((event_power_vector[i - 1]/pt0) < 0.05)){
          num_con_zeros <- num_con_zeros + 1
        }
      }
    }
  }
  return(num_con_zeros)
} 