
vector_filter <- function(data, duration, state, standards, cleaned){
  data <- filter(data, s_state==state)
  if (length(cleaned) < 2) {  data <- filter(data, clean %in% cleaned)}
  if (length(standards) < 3) {  data <- filter(data, Standard_Version %in% standards)}
  data <- filter(data, d==duration)
  return(data)
}

vector_groupby <- function(data, agg_on_standard){
  if (agg_on_standard==TRUE){
    data <- data %>% mutate(series=paste(s_state, clean))
    data <- group_by(data, ts, s_state, series)
  } else {
    data <- data %>% mutate(series=paste(Standard_Version, clean))
    data <- group_by(data, ts, s_state, Standard_Version, series)
  }
  data <- summarise(data , Power_kW=sum(power_kW))
  data <- setnames(data, c("ts", "Power_kW"), c("Time", "Power_kW"))
  data <- as.data.frame(data)
  return(data)
}