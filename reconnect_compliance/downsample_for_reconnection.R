downsample_for_reconnection <- function(post_event_response) {
  post_event_response_5s_ds <- post_event_response %>%
    group_by(c_id) %>%
    do(downsample_post_event_response(.)) %>%
    data.frame()
  
  return(post_event_response_5s_ds)
}

downsample_post_event_response <- function(post_event_response) {
  time_intervals <- seq(min(post_event_response$ts), max(post_event_response$ts), by = 5)
  avg_power <- sapply(time_intervals, function(interval) {
    mean(post_event_response$c_id_daily_norm_power[post_event_response$ts >= interval &
                                                   post_event_response$ts < interval + 5],
         na.rm = TRUE)
  })
  
  post_event_response_5S <- data.frame(
    c_id = unique(post_event_response$c_id)[1],
    ts = time_intervals,
    c_id_daily_norm_power = avg_power,
    pre_event_norm_power = unique(post_event_response$pre_event_norm_power)[1]
  )
  return(post_event_response_5S)
}
