
ufls_detection <- function(db, region, pre_event_interval, pre_event_window_length, post_event_window_length,pre_pct_sample_seconds_threshold){
  start_pre_event_window_obj <- pre_event_interval - 60 * pre_event_window_length
  start_pre_event_window_str <- format(start_pre_event_window_obj, tz = 'GMT')
  end_event_window_obj <- pre_event_interval + 60 * post_event_window_length
  end_event_window_str <- format(end_event_window_obj, tz = 'GMT')
  pre_event_interval_str <- format(pre_event_interval - 0, tz = 'GMT')
  pre_event_sample_counts <- db$get_filtered_time_series_data_all_durations(region, start_pre_event_window_str, pre_event_interval_str)
  pre_event_sample_counts <- calc_sampled_time_per_circuit(pre_event_sample_counts, start_pre_event_window_obj, pre_event_interval)
  names(pre_event_sample_counts)[names(pre_event_sample_counts) == 'sampled_seconds'] <- 'pre_event_sampled_seconds'
  post_event_sample_counts <- db$get_filtered_time_series_data_all_durations(region, pre_event_interval_str, end_event_window_str)
  post_event_sample_counts <- calc_sampled_time_per_circuit(post_event_sample_counts, pre_event_interval, end_event_window_obj)
  names(post_event_sample_counts)[names(post_event_sample_counts) == 'sampled_seconds'] <- 'post_event_sampled_seconds'
  sample_counts_by_c_id <- merge(pre_event_sample_counts, post_event_sample_counts, 
                                 by = c('c_id', 'c_id'), all = TRUE)
  sample_counts_by_c_id <- mutate(sample_counts_by_c_id, 
                                  pre_event_sampled_seconds = ifelse(is.na(pre_event_sampled_seconds), 
                                                                     0, pre_event_sampled_seconds)
                                  )
  sample_counts_by_c_id <- mutate(sample_counts_by_c_id, 
                                  post_event_sampled_seconds = ifelse(is.na(post_event_sampled_seconds), 
                                                                      0, post_event_sampled_seconds)
                                  )
  ufls_dropout <- mutate(sample_counts_by_c_id, 
    ufls_status = if_else(((pre_event_sampled_seconds/(pre_event_window_length*60) > pre_pct_sample_seconds_threshold) & 
                             (post_event_sampled_seconds == 0)),'UFLS Dropout','No UFLS Dropout')
    )
  
  return(ufls_dropout)
}

calc_sampled_time_per_circuit <- function(ts_data, start_time, end_time){
  test_sampled_points <- seq(start_time, end_time, by = "sec")
  test_sampled_points <- test_sampled_points[2:length(test_sampled_points)]
  test_sampled_points <- data.frame(test_point = test_sampled_points)
  max_duration <- max(ts_data$d) + 1
  ts_data <- mutate(ts_data, ts = fastPOSIXct(ts, tz="Australia/Brisbane"))
  ts_data <- mutate(ts_data, min_join_time = ts - max_duration)
  query <- "select * from ts_data 
            left join test_sampled_points
           where ts >= test_point and
                 test_point >= min_join_time"
  ts_data <- sqldf(query)
  ts_data <- mutate(ts_data, ts = as.POSIXct(ts, origin='1970-01-01'))
  ts_data <- mutate(ts_data, t_delta = difftime(ts, test_point, units = 'secs'))
  ts_data <- mutate(ts_data, sampled = if_else((t_delta >= 0) & (t_delta < d), 1, 0))
  ts_data <- group_by(ts_data, c_id, test_point)
  ts_data <- data.frame(summarise(ts_data, sampled = max(sampled)))
  ts_data <- group_by(ts_data, c_id)
  ts_data <- data.frame(summarise(ts_data, sampled_seconds = sum(sampled)))
  return(ts_data)
}