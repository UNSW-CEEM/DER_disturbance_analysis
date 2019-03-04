
vector_filter <- function(data, duration, state, standards, cleaned, postcodes, 
                          size_groupings, manufacturers, models){
  data <- filter(data, s_state==state)
  if (length(cleaned) < 2) {data <- filter(data, clean %in% cleaned)}
  if (length(size_groupings) < 2) {data <- filter(data, Grouping %in% size_groupings)}
  if (length(standards) < 3) {data <- filter(data, Standard_Version %in% standards)}
  if (length(postcodes) > 0) {data <- filter(data, s_postcode %in% postcodes)}
  if (length(manufacturers) > 0) {data <- filter(data, manufacturer %in% manufacturers)}
  if (length(models) > 0) {data <- filter(data, model %in% models)}
  data <- filter(data, d==duration)
  return(data)
}

vector_groupby_power <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                           manufacturer_agg, model_agg, circuit_agg, 
                           response_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==TRUE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (response_agg==TRUE){grouping_cols <- c(grouping_cols, "response_category")}
  if (circuit_agg==TRUE){grouping_cols <- c(grouping_cols, "site_id", "c_id")}
  series_cols <- grouping_cols
  grouping_cols <- c("ts", series_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , Power_kW=sum(power_kW))
  data$series <- do.call(paste, c(data[series_cols], sep = "-" ))
  data <- setnames(data, c("ts", "Power_kW"), c("Time", "Power_kW"))
  data <- as.data.frame(data)
  return(data)
}


vector_groupby_f_and_v <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                                 manufacturer_agg, model_agg, circuit_agg, 
                                 response_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==TRUE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (response_agg==TRUE){grouping_cols <- c(grouping_cols, "response_category")}
  if (circuit_agg==TRUE){grouping_cols <- c(grouping_cols, "site_id", "c_id")}
  series_cols <- grouping_cols
  grouping_cols <- c("ts", series_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data, Frequency=mean(f), Voltage=mean(v))
  data$series <- do.call(paste, c(data[series_cols], sep = "-" ))
  data <- setnames(data, c("ts"), c("Time"))
  data <- as.data.frame(data)
  return(data)
}

vector_groupby_norm_power <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                           manufacturer_agg, model_agg, circuit_agg, 
                           response_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==TRUE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (response_agg==TRUE){grouping_cols <- c(grouping_cols, "response_category")}
  if (circuit_agg==TRUE){grouping_cols <- c(grouping_cols, "site_id")}
  series_cols <- grouping_cols
  grouping_cols <- c("ts", series_cols)
  data <- group_by(data, .dots=grouping_cols)
  if ("site_id" %in% grouping_cols){
    data <- summarise(data , site_performance_factor=first(site_performance_factor))
  } else {
    data <- summarise(data , site_performance_factor=mean(site_performance_factor))
  }
  data$series <- do.call(paste, c(data[series_cols], sep = "-" ))
  data <- setnames(data, c("ts"), c("Time"))
  data <- as.data.frame(data)
  return(data)
}

vector_groupby_count <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                           manufacturer_agg, model_agg, circuit_agg, 
                           response_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==TRUE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (response_agg==TRUE){grouping_cols <- c(grouping_cols, "response_category")}
  if (circuit_agg==TRUE){grouping_cols <- c(grouping_cols, "site_id")}
  series_cols <- grouping_cols
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , sample_count=length(unique(c_id)))
  data <- as.data.frame(data)
  return(data)
}


vector_groupby_count_response <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                                 manufacturer_agg, model_agg, circuit_agg, 
                                 response_agg){
  grouping_cols <- c("clean", "response_category")
  add_cols <- c()
  if (agg_on_standard==TRUE){add_cols <- c(add_cols, "Standard_Version")}
  if (pst_agg==TRUE){add_cols <- c(add_cols, "s_postcode")}
  if (grouping_agg==TRUE){add_cols <- c(add_cols, "Grouping")}
  if (manufacturer_agg==TRUE){add_cols <- c(add_cols, "manufacturer")}
  if (model_agg==TRUE){add_cols <- c(add_cols, "model")}
  if (circuit_agg==TRUE){add_cols <- c(add_cols, "site_id")}
  grouping_cols <- c(grouping_cols, add_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , sample_count=length(unique(c_id)))
  data$series_x <- do.call(paste, c(data[c("clean", "response_category")], sep = "-" ))
  data$series_y <- do.call(paste, c(data[add_cols], sep = "-" ))
  data <- as.data.frame(data)
  data <- mutate(data, sample_count=sample_count/sum(data$sample_count))
  return(data)
}