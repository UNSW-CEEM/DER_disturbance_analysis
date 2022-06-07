
find_grouping_cols <- function(settings){
  grouping_cols <- c("clean")
  if (settings$standard_agg==TRUE){grouping_cols <- c(grouping_cols, "Standard_Version")}
  if (settings$pst_agg==TRUE){grouping_cols <- c(grouping_cols, "s_postcode")}
  if (settings$grouping_agg==TRUE){grouping_cols <- c(grouping_cols, "Grouping")}
  if (settings$manufacturer_agg==TRUE){grouping_cols <- c(grouping_cols, "manufacturer")}
  if (settings$model_agg==TRUE){grouping_cols <- c(grouping_cols, "model")}
  if (settings$response_agg==TRUE){grouping_cols <- c(grouping_cols, "response_category")}
  if (settings$zone_agg==TRUE){grouping_cols <- c(grouping_cols, "zone")}
  if (settings$compliance_agg==TRUE){grouping_cols <- c(grouping_cols, "compliance_status")}
  if (settings$compliance_2020_agg==TRUE){grouping_cols <- c(grouping_cols, "compliance_status_2020")}
  if (settings$reconnection_compliance_agg==TRUE){grouping_cols <- c(grouping_cols, "reconnection_compliance_status")}
  if (settings$circuit_agg==TRUE){grouping_cols <- c(grouping_cols, "site_id", "c_id")}
  return(grouping_cols)
}
  
vector_groupby_power <- function(data, grouping_cols){
  series_cols <- grouping_cols
  grouping_cols <- c("ts", series_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , Power_kW=sum(power_kW, na.rm = TRUE))
  data$series <- do.call(paste, c(data[series_cols], sep = "-" ))
  data <- setnames(data, c("ts", "Power_kW"), c("Time", "Power_kW"))
  data <- as.data.frame(data)
  return(data)
}

vector_groupby_f_and_v <- function(data, grouping_cols){
  series_cols <- grouping_cols
  grouping_cols <- c("ts", series_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data, Frequency=mean(f, na.rm = TRUE), Voltage=mean(v, na.rm = TRUE))
  data$series <- do.call(paste, c(data[series_cols], sep = "-" ))
  data <- setnames(data, c("ts"), c("Time"))
  data <- as.data.frame(data)
  return(data)
}

vector_groupby_norm_power <- function(data, grouping_cols){
  series_cols <- grouping_cols
  grouping_cols <- c("ts", series_cols)
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , c_id_norm_power=mean(c_id_norm_power, na.rm = TRUE))
  data$series <- do.call(paste, c(data[series_cols], sep = "-" ))
  data <- setnames(data, c("ts"), c("Time"))
  data <- as.data.frame(data)
  return(data)
}

vector_groupby_count <- function(data, grouping_cols){
  grouping_cols <- grouping_cols[grouping_cols != "c_id"]
  series_cols <- grouping_cols
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , sample_count=length(unique(c_id)))
  data <- as.data.frame(data)
  return(data)
}

vector_groupby_count_response <- function(data, grouping_cols){
  if (!"response_category" %in% grouping_cols) {grouping_cols <- c(grouping_cols, "response_category")}
  grouping_cols <- grouping_cols[grouping_cols != "c_id"]
  add_cols <- grouping_cols[!grouping_cols %in% c("clean", "response_category")]
  data <- group_by(data, .dots=grouping_cols)
  data <- summarise(data , sample_count=length(unique(c_id)))
  data$series_x <- do.call(paste, c(data[c("response_category", "clean")], sep = "-" ))
  if (length(add_cols) >= 1){
    data$series_y <- do.call(paste, c(data[add_cols], sep = "-" ))
  } else {
    data <- mutate(data, series_y="All")
  }
  data <- as.data.frame(data)
  data <- mutate(data, sample_count=sample_count/sum(data$sample_count))
  return(data)
}

vector_groupby_count_zones <- function(data, grouping_cols){
  grouping_cols <- c("clean", "response_category")
  category_size <- group_by(data, clean, zone)
  category_size <- summarise(category_size , category_count=length(unique(c_id)))
  category_size <- as.data.frame(category_size)
  if (!"zone" %in% grouping_cols) {grouping_cols <- c(grouping_cols, "zone")}
  grouping_cols <- grouping_cols[grouping_cols != "c_id"]
  add_cols <- grouping_cols[!grouping_cols %in% c("clean", "zone")]
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
  data <- left_join(data, category_size, by=c("clean", "zone"))
  data <- mutate(data, sample_count=sample_count/category_count)
  return(data)
}

vector_groupby_cumulative_distance <- function(data, grouping_cols){
  grouping_cols <- grouping_cols[!grouping_cols %in% c("zone", "s_postcode", "site_id", "c_id")]
  series_cols <- grouping_cols
  grouping_cols <- series_cols
  data <- data[!is.na(data$s_postcode),]
  data <- distinct(data, clean, c_id, .keep_all=TRUE)
  data <- mutate(data, num_disconnects=ifelse(response_category %in% c("4 Disconnect", "3 Drop to Zero"),1,0))
  data <- mutate(data, system_count=1)
  data <- data[order(data$distance),]
  data <- group_by(data, .dots=c(grouping_cols, "s_postcode"))
  data <- summarise(data, distance=first(distance), num_disconnects=sum(num_disconnects), 
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

vector_groupby_system <- function(data, grouping_cols){
  grouping_cols <- grouping_cols[!grouping_cols %in% c("site_id", "c_id")]
  series_cols <- grouping_cols
  if ("clean" %in% data$clean){
    data = filter(data, clean=="clean")
  } else {
    data = filter(data, clean=="raw")
  }
  data <- distinct(data, c_id, .keep_all=TRUE)
  data <- mutate(data, num_disconnects=ifelse(response_category %in% c("4 Disconnect", "3 Drop to Zero"),1,0))
  data <- mutate(data, system_count=1)
  data <- group_by(data, .dots=c("s_postcode"))
  data <- summarise(data , num_disconnects=sum(num_disconnects), 
                    system_count=sum(system_count), lat=first(lat), lon=first(lon))
  data <- mutate(data, percentage_disconnect=round(num_disconnects/system_count, digits=2))
  data <- as.data.frame(data)
  data$series <- data$percentage_disconnect
  data$info <- do.call(paste, c(data[c("s_postcode", "percentage_disconnect", "system_count")], sep = "-" ))
  return(data)
}

