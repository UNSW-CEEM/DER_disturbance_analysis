
get_distance_from_event <- function(combined_data, postcode_data, event_lat, 
                                    event_lon){
  postcode_data <- filter(postcode_data, !is.na(lat) & !is.na(long))
  postcode_data <- mutate(postcode_data, lat=as.numeric(lat))
  postcode_data <- mutate(postcode_data, lon=as.numeric(long))
  distances <- get_postcode_distance_from_event(postcode_data, event_lat, 
                                               event_lon)
  combined_data <- left_join(combined_data, distances, 
                              by=c("s_postcode" = "postcode"))
  return(combined_data)
}

get_postcode_distance_from_event <- function(postcode_data, event_lat, 
                                             event_lon){
  postcode_data <- mutate(postcode_data, event_lat=event_lat)
  postcode_data <- mutate(postcode_data, event_lon=event_lon)
  postcode_data <- postcode_data %>%  mutate(
     distance = distHaversine(cbind(postcode_data$lon, postcode_data$lat),
                              cbind(postcode_data$event_lon, postcode_data$event_lat))/1000)
  distances <- select(postcode_data, postcode, distance)
  return(distances)
}
