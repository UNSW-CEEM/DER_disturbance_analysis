
vector_filter <- function(data, duration, state, standards){
  column_value_pairs <- list(list('d', duration), list('s_state', state),
                             list('Standard_Version', standards))
  for (column_value_pair in column_value_pairs) {
    data <- eval(parse(text = paste("filter(data,", column_value_pair[[1]], 
                                    "%in% column_value_pair[[2]] )"))) 
  }
  return(data)
}

vector_groupby <- function(data, agg_on_standard){
  if (agg_on_standard==TRUE){
    data <- data %>% mutate(series=s_state)
    data <- group_by(data, ts, s_state, series)
  } else {
    data <- data %>% mutate(series=Standard_Version)
    data <- group_by(data, ts, s_state, Standard_Version, series)
  }
  data <- summarise(data , Power_kW=sum(power_kW))
  data <- setnames(data, c("ts", "Power_kW"), c("Time", "Power_kW"))
  data <- as.data.frame(data)
  return(data)
}