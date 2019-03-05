
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
                           response_agg, zone_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==TRUE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (response_agg==TRUE){grouping_cols <- c(grouping_cols, "response_category")}
  if (zone_agg==TRUE){grouping_cols <- c(grouping_cols, "zone")}
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
                                 response_agg, zone_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==TRUE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (response_agg==TRUE){grouping_cols <- c(grouping_cols, "response_category")}
  if (zone_agg==TRUE){grouping_cols <- c(grouping_cols, "zone")}
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
                           response_agg, zone_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==TRUE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (response_agg==TRUE){grouping_cols <- c(grouping_cols, "response_category")}
  if (zone_agg==TRUE){grouping_cols <- c(grouping_cols, "zone")}
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
                           response_agg, zone_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (pst_agg==TRUE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (response_agg==TRUE){grouping_cols <- c(grouping_cols, "response_category")}
  if (zone_agg==TRUE){grouping_cols <- c(grouping_cols, "zone")}
  if (circuit_agg==TRUE){grouping_cols <- c(grouping_cols, "site_id")}
  series_cols <- grouping_cols
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , sample_count=length(unique(c_id)))
  data <- as.data.frame(data)
  return(data)
}

vector_groupby_count_response <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                                 manufacturer_agg, model_agg, circuit_agg, 
                                 response_agg, zone_agg){
  grouping_cols <- c("clean", "response_category")
  add_cols <- c()
  if (agg_on_standard==TRUE){add_cols <- c(add_cols, "Standard_Version")}
  if (pst_agg==TRUE){add_cols <- c(add_cols, "s_postcode")}
  if (grouping_agg==TRUE){add_cols <- c(add_cols, "Grouping")}
  if (manufacturer_agg==TRUE){add_cols <- c(add_cols, "manufacturer")}
  if (model_agg==TRUE){add_cols <- c(add_cols, "model")}
  if (zone_agg==TRUE){add_cols <- c(add_cols, "zone")}
  if (circuit_agg==TRUE){add_cols <- c(add_cols, "site_id")}
  grouping_cols <- c(grouping_cols, add_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , sample_count=length(unique(c_id)))
  data$series_x <- do.call(paste, c(data[c("clean", "response_category")], sep = "-" ))
  if (length(add_cols) >= 1){
    data$series_y <- do.call(paste, c(data[add_cols], sep = "-" ))
  } else {
    data <- mutate(data, series_y="All")
    }
  data <- as.data.frame(data)
  data <- mutate(data, sample_count=sample_count/sum(data$sample_count))
  return(data)
}

vector_groupby_count_zones <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                                          manufacturer_agg, model_agg, circuit_agg, 
                                          response_agg, zone_agg){
  grouping_cols <- c("clean", "zone")
  add_cols <- c()
  if (agg_on_standard==TRUE){add_cols <- c(add_cols, "Standard_Version")}
  if (pst_agg==TRUE){add_cols <- c(add_cols, "s_postcode")}
  if (grouping_agg==TRUE){add_cols <- c(add_cols, "Grouping")}
  if (manufacturer_agg==TRUE){add_cols <- c(add_cols, "manufacturer")}
  if (model_agg==TRUE){add_cols <- c(add_cols, "model")}
  if (response_agg==TRUE){add_cols <- c(add_cols, "response_category")}
  if (circuit_agg==TRUE){add_cols <- c(add_cols, "site_id")}
  grouping_cols <- c(grouping_cols, add_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , sample_count=length(unique(c_id)))
  data$series_x <- do.call(paste, c(data[c("clean", "zone")], sep = "-" ))
  if (length(add_cols) >= 1){
    data$series_y <- do.call(paste, c(data[add_cols], sep = "-" ))
  } else {
    data <- mutate(data, series_y="All")
  }
  data <- as.data.frame(data)
  data <- mutate(data, sample_count=sample_count/sum(data$sample_count))
  return(data)
}

vector_groupby_cumulative_distance <- function(data, agg_on_standard, pst_agg, grouping_agg, 
                                 manufacturer_agg, model_agg, circuit_agg, 
                                 response_agg){
  grouping_cols <- c("clean")
  if (agg_on_standard==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (circuit_agg==TRUE){grouping_cols <- c(grouping_cols, "site_id", "c_id")}
  series_cols <- grouping_cols
  grouping_cols <- series_cols
  data <- mutate(data, num_disconnects=ifelse(response_category %in% c("Disconnect", "Drop to Zero"),1,0))
  data <- mutate(data, system_count=1)
  data <- data[order(data$distance),]
  data <- group_by(data, .dots=c(grouping_cols, "s_postcode"))
  data <- summarise(data ,  distance=first(distance), 
                    num_disconnects=sum(num_disconnects),
                    system_count=sum(system_count))
  data <- as.data.frame(data)
  data <- data[order(data$distance),]
  data <- group_by(data, .dots=grouping_cols)
  data <- mutate(data,num_disconnects=cumsum(num_disconnects))
  data <- mutate(data, system_count=cumsum(system_count))
  data <- as.data.frame(data)
  data$series <- do.call(paste, c(data[series_cols], sep = "-" ))
  data <- mutate(data, percentage=num_disconnects/system_count)
  return(data)
}



