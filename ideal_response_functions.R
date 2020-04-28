
ideal_response <- function(frequency_data){
  frequency_data <- frequency_data[order(frequency_data$ts),]
  start_times <- c()
  end_times <- c()
  ts <- c()
  f <- c()
  norm_power <- c()
  for(i in 1:length(frequency_data$ts)){
    last_60_seconds_of_data <- filter(frequency_data, (ts>frequency_data$ts[i]-60) & (ts<=frequency_data$ts[i]))
    if(frequency_data$f[i]>=50.25 & length(start_times)==length(end_times)){
      start_times <- c(start_times, frequency_data$ts[i])
      if(length(ts)==0){
        ts <- c(frequency_data$ts[i])
      }else{
          ts <- c(ts, frequency_data$ts[i])
      }
      f <- c(f, frequency_data$f[i])
      norm_power <- c(norm_power, 1)
    } else if(max(last_60_seconds_of_data$f)<=50.15 & length(start_times)>length(end_times)){
      end_times <- c(end_times, frequency_data$ts[i])
      ts <- c(ts, frequency_data$ts[i])
      norm_power <- c(norm_power, tail(norm_power, n=1))
      f <- c(f, frequency_data$f[i])
    } else if(length(start_times)>length(end_times)){
      ts <- c(ts, frequency_data$ts[i])
      f <- c(f, frequency_data$f[i])
      if(frequency_data$f[i]>=50.25){
        norm_power <- c(norm_power, min(norm_p_over_frequency(frequency_data$f[i]),tail(norm_power, n=1)))
      } else {
        norm_power <- c(norm_power, tail(norm_power, n=1))
      }
    }
  }
  response_data <- data.frame(ts, f, norm_power, stringsAsFactors=FALSE)
  return(response_data)
}

norm_p_over_frequency <- function(f){
  if(f>=50.25 & f<=52.00){
    norm_p <- 1 - ((f-50.25)/(52-50.25))
  } else if (f>52){
    norm_p <- 0.0
  } else {
    norm_p = 1.0
  }
  return((norm_p))
}

down_sample_1s <- function(ideal_response_1_s, duration, offset){
  ideal_response_1_s <- thicken(ideal_response_1_s, paste(duration,'s'), colname='time_group', rounding='up',
                                start_val=offset - as.numeric(duration))
  ideal_response_1_s <- thicken(ideal_response_1_s, paste(duration,'s'), colname='time_group2', rounding='down', 
                                by='ts', start_val=offset - as.numeric(duration))
  ideal_response_1_s[ideal_response_1_s$ts==ideal_response_1_s$time_group2,]$time_group <- 
    ideal_response_1_s[ideal_response_1_s$ts==ideal_response_1_s$time_group2,]$time_group2
  ideal_response_1_s <- filter(
    ideal_response_1_s, 
    ts<=max(ideal_response_1_s[ideal_response_1_s$ts==ideal_response_1_s$time_group2,]$time_group2))
  ideal_response_1_s <- filter(
    ideal_response_1_s, 
    ts>=min(ideal_response_1_s[ideal_response_1_s$ts==(ideal_response_1_s$time_group2+1),]$ts))
  ideal_response_downsampled <- group_by(ideal_response_1_s, time_group)
  ideal_response_downsampled <- summarise(ideal_response_downsampled, f=last(f), norm_power=mean(norm_power))
  ideal_response_downsampled <- data.frame(ideal_response_downsampled)
  return(ideal_response_downsampled)
}

calc_error_metric_and_compliance <- function(combined_data, ideal_response_downsampled){
  error_by_c_id <- calc_error_metric(combined_data, ideal_response_downsampled)
  threshold_error <- calc_threshold_error(ideal_response_downsampled)
  error_by_c_id <- calc_compliance_status(error_by_c_id, threshold_error)
  combined_data <- left_join(combined_data, error_by_c_id, by="site_id")
  return(combined_data)
}

calc_error_metric <- function(combined_data, ideal_response_downsampled){
  combined_data <- distinct(combined_data, site_id, ts, .keep_all=TRUE)
  combined_data <- inner_join(combined_data, ideal_response_downsampled, by=c("ts"="time_group"))
  combined_data <- mutate(combined_data, 
                          abs_percent_diff_actual_cf_ideal=abs(Site_Event_Normalised_Power_kW-norm_power)/norm_power)
  combined_data <- mutate(combined_data, percent_diff_actual_cf_ideal=(Site_Event_Normalised_Power_kW-norm_power)/norm_power)
  error_by_c_id <- group_by(combined_data, site_id)
  error_by_c_id <- summarise(error_by_c_id, 
                             min_diff=min(percent_diff_actual_cf_ideal),
                             max_diff=max(percent_diff_actual_cf_ideal),
                             abs_percent_diff_actual_cf_ideal=mean(abs_percent_diff_actual_cf_ideal),
                             percent_diff_actual_cf_ideal=mean(percent_diff_actual_cf_ideal))
  error_by_c_id <- data.frame(error_by_c_id)
  error_by_c_id <- mutate(error_by_c_id, mixed_wrt_spec=1)
  error_by_c_id <- mutate(error_by_c_id, below_spec=ifelse(max_diff<=0,1,0))
  error_by_c_id <- mutate(error_by_c_id, above_spec=ifelse(min_diff>=0,1,0))
  error_by_c_id <- mutate(error_by_c_id, mixed_wrt_spec=mixed_wrt_spec-below_spec-above_spec)
  error_by_c_id <- mutate(error_by_c_id, 
                          combined_error_metric=(below_spec+above_spec) * percent_diff_actual_cf_ideal
                          + mixed_wrt_spec * abs_percent_diff_actual_cf_ideal)
  return(error_by_c_id)
}

calc_compliance_status <- function(error_by_c_id, threshold_error){
  error_by_c_id <- mutate(error_by_c_id, compliance_status=ifelse(below_spec==1,'Compliant','Undefined'))
  error_by_c_id <- mutate(error_by_c_id, compliance_status=ifelse(above_spec==1,'Above Ideal Response',compliance_status))
  error_by_c_id <- mutate(error_by_c_id, compliance_status=ifelse(above_spec==1 & combined_error_metric>threshold_error,
                                                                  'Non Complinant',compliance_status))
  error_by_c_id <- mutate(error_by_c_id, compliance_status=ifelse(mixed_wrt_spec==1, 'Ambigous', compliance_status))
  return(error_by_c_id)
}

calc_threshold_error <- function(ideal_response_downsampled){
  threshold_error <- mean((1 - ideal_response_downsampled$norm_power)/ideal_response_downsampled$norm_power)
  return(threshold_error)
}

calc_error_metric_and_compliance_2 <- function(combined_data, ideal_response_downsampled, ideal_response,
                                               threshold, start_buffer, end_buffer, end_buffer_responding, disconnecting_threshold){
  start_buffer_t <- min(ideal_response$ts) + start_buffer
  end_buffer <- max(ideal_response$ts) - end_buffer
  end_buffer_responding <- min(ideal_response$ts) + end_buffer_responding
  disconnecting_threshold <- disconnecting_threshold
  
  # First pass compliance
  ideal_response_downsampled <- filter(ideal_response_downsampled, time_group >= start_buffer_t)
  ideal_response_downsampled <- filter(ideal_response_downsampled, time_group <= end_buffer)
  error_by_c_id <- inner_join(combined_data, ideal_response_downsampled, by=c("ts"="time_group"))
  error_by_c_id <- mutate(error_by_c_id, error=(1 - c_id_norm_power) - ((1 - norm_power) * threshold))
  error_by_c_id <- group_by(error_by_c_id, c_id, clean)
  error_by_c_id <- summarise(error_by_c_id, min_error=min(error))
  error_by_c_id <- mutate(error_by_c_id, compliance_status=ifelse(min_error>=0.0, 'Compliant', 'Non-compliant'))
  error_by_c_id <- select(error_by_c_id, c_id, compliance_status, clean)
  combined_data <- left_join(combined_data, error_by_c_id, by=c("c_id","clean"))
  
  # Change 'Non compliant' to 'Non Compliant Responding' where complaint at start
  ideal_response_downsampled <- filter(ideal_response_downsampled, time_group <= end_buffer_responding)
  error_by_c_id <- inner_join(combined_data, ideal_response_downsampled, by=c("ts"="time_group"))
  error_by_c_id <- mutate(error_by_c_id, error=(1 - c_id_norm_power) - ((1 - norm_power) * threshold))
  error_by_c_id <- group_by(error_by_c_id, c_id, clean)
  error_by_c_id <- summarise(error_by_c_id, max_error=max(error), compliance_status=first(compliance_status))
  error_by_c_id <- mutate(error_by_c_id, compliance_status=ifelse((max_error>=0.0) & (compliance_status=='Non-compliant'), 'Non-compliant Responding', compliance_status))
  error_by_c_id <- select(error_by_c_id, c_id, compliance_status, clean)
  combined_data <- left_join(subset(combined_data, select = -c(compliance_status)), error_by_c_id, by=c("c_id","clean"))
  
  # Set disconnecting categories 
  min_ideal_response <- min(ideal_response_downsampled$norm_power)
  if (min_ideal_response > disconnecting_threshold) {
    combined_data <- mutate(combined_data, compliance_status=ifelse(response_category=='4 Disconnect', 'Disconnect/Drop to Zero', compliance_status))
    combined_data <- mutate(combined_data, compliance_status=ifelse(response_category=='3 Drop to Zero', 'Disconnect/Drop to Zero', compliance_status))
    combined_data <- mutate(combined_data, compliance_status=ifelse(response_category=='5 Off at t0', 'Off at t0', compliance_status))
    combined_data <- mutate(combined_data, compliance_status=ifelse(response_category=='6 Not enough data', 'Not enough data', compliance_status))
  } 
  
  return(combined_data)
}
