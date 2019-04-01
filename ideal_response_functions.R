
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