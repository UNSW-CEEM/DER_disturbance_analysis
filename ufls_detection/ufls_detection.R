
ufls_detection <- function(db, region, pre_event_interval, event_window_length, pct_sample_reduction_threshold){
  start_pre_event_window <- pre_event_interval - 60 * event_window_length
  start_pre_event_window <- format(as.POSIXct(start_pre_event_window, tz = "Australia/Brisbane"), tz = 'GMT')
  end_event_window <- pre_event_interval + 60 * event_window_length
  end_event_window <- format(as.POSIXct(end_event_window, tz = "Australia/Brisbane"), tz = 'GMT')
  pre_event_sample_counts <- db$get_samples_per_circuit(region, start_pre_event_window, pre_event_interval)
  names(pre_event_sample_counts)[names(pre_event_sample_counts) == 'samples'] <- 'pre_event_count'
  post_event_sample_counts <- db$get_samples_per_circuit(region, pre_event_interval, end_event_window)
  names(post_event_sample_counts)[names(post_event_sample_counts) == 'samples'] <- 'post_event_count'
  sample_counts_by_c_id <- merge(pre_event_sample_counts, post_event_sample_counts, 
                                 by = c('c_id', 'c_id'), all = TRUE)
  sample_counts_by_c_id <- mutate(sample_counts_by_c_id, pre_event_count = ifelse(is.na(pre_event_count), 
                                                                                 0, pre_event_count))
  sample_counts_by_c_id <- mutate(sample_counts_by_c_id, post_event_count = ifelse(is.na(post_event_count), 
                                                                                  0, post_event_count))
  ufls_dropout <- mutate(sample_counts_by_c_id, 
                          ufls_status = if_else((post_event_count/pre_event_count) < pct_sample_reduction_threshold,
                                  'UFLS Dropout', 'No UFLS Dropout'))
  return(ufls_dropout)
}