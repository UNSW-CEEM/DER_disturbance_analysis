
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
                           manufacturer_agg, model_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==FALSE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==FALSE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==FALSE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==FALSE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==FALSE){grouping_cols <- c(grouping_cols, "model")}
  series_cols <- grouping_cols
  grouping_cols <- c("ts", series_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , Power_kW=sum(power_kW))
  data$series <- do.call(paste, c(data[series_cols], sep = "-" ))
  data <- setnames(data, c("ts", "Power_kW"), c("Time", "Power_kW"))
  data <- as.data.frame(data)
  return(data)
}

vector_groupby_norm_power <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                           manufacturer_agg, model_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==FALSE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==FALSE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==FALSE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==FALSE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==FALSE){grouping_cols <- c(grouping_cols, "model")}
  series_cols <- grouping_cols
  grouping_cols <- c("ts", series_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , Event_Normalised_Power_kW=mean(na.omit(normalised_power_kW)),
                    Frequency=mean(na.omit(f)),Voltage=mean(na.omit(v)))
  data$series <- do.call(paste, c(data[series_cols], sep = "-" ))
  data <- setnames(data, c("ts"), c("Time"))
  data <- as.data.frame(data)
  return(data)
}

vector_groupby_count <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                           manufacturer_agg, model_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==FALSE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==FALSE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==FALSE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==FALSE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==FALSE){grouping_cols <- c(grouping_cols, "model")}
  series_cols <- grouping_cols
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , sample_count=length(unique(site_id)))
  data <- as.data.frame(data)
  return(data)
}