# Finding percentage disconnected/Dropped to Zero DPVs at specific voltage ranges

calc_percentage_disconnect_or_droptozero_DPVs <- function(circuit_summary, combined_data_filtered){
  ts_data <- combined_data_filtered

  data_for_voltage_visualization <- data.frame(response_category = circuit_summary$response_category, zone = circuit_summary$zone,
                                                           v_min = circuit_summary$vmin_min,v_max=circuit_summary$vmax_max )
  find_rows_disconnected_or_droptozero <- filter(data_for_voltage_visualization,
                                                 data_for_voltage_visualization$response_category == '4 Disconnect' | data_for_voltage_visualization$response_category == '3 Drop to Zero')

  circuit_counts_for_voltage <- find_rows_disconnected_or_droptozero %>%
    mutate(Voltage_Category = case_when(
      v_min < 180 ~ "Vmin<180",
      v_min >= 180 & v_min < 200 ~ "180V \u2264 Vmin < 200V", 
      v_min >= 200 & v_min < 220 ~ "200V \u2264 Vmin < 220V",
      v_min >= 220 & v_min < 240 ~ "220V \u2264 Vmin < 240V",
      v_min >= 240 & v_min < 260 ~ "240V \u2264 Vmin < 260V",
      v_max >= 260 & v_max < 265 ~ "260V \u2264 Vmax < 265V",
      v_max >= 265 ~ "Vmax \u2265 265V",
      v_min < 180 & v_max > 265 ~ "Vmin < 180V & Vmax > 265V",
    )) %>%
    group_by(zone, Voltage_Category) %>%
    summarise(Count_ccts = n()) %>%
    ungroup()
  
    circuit_counts_for_voltage <- data.frame(circuit_counts_for_voltage)
    circuit_counts_for_voltage <- circuit_counts_for_voltage[complete.cases(circuit_counts_for_voltage$Voltage_Category), ] # remove NAs

    total_disconnect_droptozero_counts_per_zone <- circuit_counts_for_voltage %>%
      group_by(zone) %>%
      summarise(total_count_disconnect = sum(Count_ccts))

    total_counts_per_zone <- data_for_voltage_visualization %>%
      group_by(zone) %>%
      summarise(total_count = n())

    merged_data_voltage_visualization <- merge(total_counts_per_zone, total_disconnect_droptozero_counts_per_zone, by = "zone", all = TRUE)
    merged_data_voltage_visualization <- merge(merged_data_voltage_visualization, circuit_counts_for_voltage, by = "zone", all = TRUE)
    
    merged_data_voltage_visualization$Percentage <- (merged_data_voltage_visualization$Count_ccts / merged_data_voltage_visualization$total_count) * 100
    data_to_plot <- merged_data_voltage_visualization[, c("zone", "Voltage_Category", "Percentage")]
    
     return(data_to_plot)
}    
