summarise_disconnections <- function(circuit_summary, sample_threshold){
  
  # Don't count circuits without a well defined response type.
  bad_categories <- c("6 Not enough data", "Undefined")
  circuit_summary <- filter(circuit_summary, !(response_category %in% bad_categories | is.na(response_category)))
  
  # Get an initial summary of disconnection count and sample size by manufacturer.
  disconnection_summary <- group_by(circuit_summary, Standard_Version, manufacturer)
  disconnection_summary <- summarise(disconnection_summary, 
                                     disconnections = get_number_of_disconnections(response_category),
                                     total = length(response_category))
  
  # Create an Other group for manufacturers with a small sample size.
  disconnection_summary <- mutate(disconnection_summary, 
                                  manufacturer = ifelse(total < sample_threshold | 
                                                          manufacturer == "Unknown" | 
                                                          manufacturer == "Multiple", 
                                                        "Other", manufacturer)
                                  )
  
  # Recalculate disconnection count and sample size. 
  disconnection_summary <- group_by(disconnection_summary, Standard_Version, manufacturer)
  disconnection_summary <- summarise(disconnection_summary, 
                                     disconnections = sum(disconnections),
                                     total = sum(total))
  
  return(disconnection_summary)
}

get_number_of_disconnections <- function(response_categories){
  disconnection_categories <- c("3 Drop to Zero", "4 Disconnect")
  response_categories <- response_categories[response_categories %in% disconnection_categories]
  return(length(response_categories))
}