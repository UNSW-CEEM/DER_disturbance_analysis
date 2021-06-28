
ufls_detection <- function(db, region, pre_event_interval, event_window_length, pct_sample_reduction_threshold){
  start_pre_event_window <- pre_event_interval - 60 * event_window_length
  start_pre_event_window <- format(start_pre_event_window, tz = 'GMT')
  end_event_window <- pre_event_interval + 60 * event_window_length
  end_event_window <- format(end_event_window, tz = 'GMT')
  pre_event_interval_str <- format(pre_event_interval - 0, tz = 'GMT')
  pre_event_sample_counts <- db$get_sampled_time_per_circuit(region, start_pre_event_window, pre_event_interval_str)
  names(pre_event_sample_counts)[names(pre_event_sample_counts) == 'sampled_seconds'] <- 'pre_event_sampled_seconds'
  post_event_sample_counts <- db$get_sampled_time_per_circuit(region, pre_event_interval_str, end_event_window)
  names(post_event_sample_counts)[names(post_event_sample_counts) == 'sampled_seconds'] <- 'post_event_sampled_seconds'
  sample_counts_by_c_id <- merge(pre_event_sample_counts, post_event_sample_counts, 
                                 by = c('c_id', 'c_id'), all = TRUE)
  sample_counts_by_c_id <- mutate(sample_counts_by_c_id, pre_event_sampled_seconds = ifelse(is.na(pre_event_sampled_seconds), 
                                                                                 0, pre_event_sampled_seconds))
  sample_counts_by_c_id <- mutate(sample_counts_by_c_id, post_event_sampled_seconds = ifelse(is.na(post_event_sampled_seconds), 
                                                                                  0, post_event_sampled_seconds))
  ufls_dropout <- mutate(sample_counts_by_c_id, 
                          ufls_status = if_else((post_event_sampled_seconds/pre_event_sampled_seconds) < pct_sample_reduction_threshold,
                                  'UFLS Dropout', 'No UFLS Dropout'))
  return(ufls_dropout)
}