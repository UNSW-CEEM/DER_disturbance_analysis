
get_distance_from_event <- function(combined_data, postcode_data, event_lat, event_lon) {
  distances <- get_postcode_distance_from_event(postcode_data, event_lat, event_lon)
  combined_data <- left_join(combined_data, distances, by = c("s_postcode" = "postcode"))
  return(combined_data)
}

get_postcode_distance_from_event <- function(postcode_data, event_lat, event_lon) {
  distances <- postcode_data %>%
    mutate(
      distance = distHaversine(cbind(postcode_data$lon, postcode_data$lat), cbind(event_lon, event_lat)) / 1000
    ) %>%
    select(postcode, distance, lat, lon)
  return(distances)
}

get_zones <- function(combined_data, radius_one, radius_two, radius_three) {
  combined_data <- combined_data %>%
    mutate(zone = ifelse(distance < radius_one, "1 Zone", "4 Undefined")) %>%
    mutate(zone = ifelse(distance < radius_two & zone == "4 Undefined", "2 Zone", zone)) %>%
    mutate(zone = ifelse(distance < radius_three & zone == "4 Undefined", "3 Zone", zone))
  return(combined_data)
}
