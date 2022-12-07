
impose_sample_size_threshold_compliance <- function(Proportions, sample_threshold){
  
  # Add OEM's to 'Other' if no corresponding capacity in CER
  Proportions <- Proportions %>% ungroup(manufacturer) %>% mutate(sample_threshold)
  
  Proportions <- mutate(Proportions,manufacturer = ifelse(is.na(Count), 
                                                          'Other', manufacturer))
  # Add manufacturers with a small sample size to Other group.
  Proportions <- mutate(Proportions, Sample_size = ifelse(is.na(Sample_size), 0, Sample_size))
  
  Proportions <- mutate(Proportions, manufacturer = ifelse(is.na(capacity), 
                                                           'Other', manufacturer))
  Proportions <- mutate(Proportions, 
                        manufacturer = ifelse(Sample_size < sample_threshold | 
                                                manufacturer == "Unknown" | 
                                                manufacturer == "Multiple" |
                                                manufacturer == "Mixed" | 
                                                manufacturer == "" |
                                                is.na(manufacturer), 
                                              "Other", manufacturer)
  )
  # Recalculate disconnection count and sample size. 
  Proportions <- group_by(Proportions, Standard_Version, manufacturer, StdComplianceCombined)
  Proportions <- summarise(Proportions, 
                           Count = sum(Count, na.rm = TRUE),
                           Sample_size = sum(Sample_size, na.rm = TRUE),
                           capacity = sum(capacity, na.rm = TRUE))
  
  # overwrite "Other capacity"
  cer_install_data <- filter(cer_install_data, state == region)
  total_install_on_event_date <- sum(filter(cer_install_data, date == min(event_date, last(cer_install_data$date)))$capacity)
  total_install_2005_std <- sum(filter(cer_install_data, date == "2016-10-16")$capacity)
  total_install_2015_std <- sum(filter(cer_install_data, date == "2022-01-01")$capacity) - total_install_2005_std
  total_install_2020_std <- total_install_on_event_date - total_install_2015_std - total_install_2005_std
  
  Standard_Capacity <- c(total_install_2015_std, total_install_2020_std)
  Standard_Version <- c("AS4777.2:2015", "AS4777.2:2020")
  
  Standard_cap <- data.frame(Standard_Version, Standard_Capacity)
  Standard_cap$Standard_Version <- as.character(Standard_cap$Standard_Version)
  
  # 
  # Set 'other' cap to be difference of total install of standard and the OEMS with > 30 samples'
  Proportions <- mutate(Proportions, capacity = ifelse(manufacturer == "Other", 0, capacity))
  
  Capacity_OEM <- group_by(Proportions, Standard_Version, manufacturer, capacity) %>% summarise()
  Capacity_OEM <- group_by(Capacity_OEM, Standard_Version) %>% summarise(capacity_OEM = sum(capacity))
  
  Capacities <- left_join(Standard_cap, Capacity_OEM, by = "Standard_Version")
  Other_capacity <- mutate(Capacities, other_capacity = Standard_Capacity - capacity_OEM)
  # 
  Proportions <- left_join(Proportions, Other_capacity[c("Standard_Version", "other_capacity")], by = "Standard_Version")
  Proportions <- mutate(Proportions, capacity = ifelse(manufacturer == "Other", other_capacity, capacity))
  
  Proportions <- mutate(Proportions, proportion = Count / Sample_size)
  Proportions <- data.frame(Proportions)
  return(Proportions)
}


combine_erroneous_OEMs <- function(df) {
  
  
  # manufacturers to 'other'
  df <- mutate(df, manufacturer = ifelse(is.na(proportion_capacity),
                                         'Other', manufacturer))
  # Also rename any manufacturers that may have been missed as 'other' (probably redundant after the step above??)
  df <- mutate(df, 
               manufacturer = ifelse(manufacturer == "Unknown" | 
                                       manufacturer == "Multiple" |
                                       manufacturer == "Mixed" | 
                                       manufacturer == "" |
                                       is.na(manufacturer), 
                                     "Other", manufacturer))
  return(df)
}



