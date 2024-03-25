# Finding percentage disconnected/Dropped to Zero DPVs at specific voltage ranges

calc_percentage_disconnect_or_droptozero_DPVs <- function(circuit_summary, combined_data_filtered, duration){
  
  find_rows_disconnected_or_droptozero <- filter(circuit_summary,
                                                 circuit_summary$response_category == '4 Disconnect' | circuit_summary$response_category == '3 Drop to Zero')
  
  circuit_counts_for_voltage <- find_rows_disconnected_or_droptozero %>%
    mutate(Voltage_Category = case_when(
      is.na(vmin_min) ~ "No voltage data",
      vmin_min >= 180 & vmin_min < 200 ~ "180V \u2264 Vmin < 200V", 
      vmin_min >= 200 & vmin_min < 220 ~ "200V \u2264 Vmin < 220V",
      vmin_min >= 220 & vmin_min < 240 ~ "220V \u2264 Vmin < 240V",
      vmin_min >= 240 & vmin_min < 260 ~ "240V \u2264 Vmin < 260V",
      vmax_max >= 260 & vmax_max < 265 ~ "260V \u2264 Vmax < 265V",
      vmax_max >= 265 ~ "Vmax \u2265 265V",
      vmin_min < 180 & vmax_max > 265 ~ "Vmin < 180V & Vmax > 265V",
    )) %>%
    group_by(zone, Voltage_Category) %>%
    summarise(Count_ccts = n()) %>%
    ungroup()
  
  circuit_counts_for_voltage <- data.frame(circuit_counts_for_voltage)
  
  total_disconnect_droptozero_counts_per_zone <- circuit_counts_for_voltage %>%
    group_by(zone) %>%
    summarise(total_count_disconnect = sum(Count_ccts))
  
  total_counts_per_zone <- circuit_summary %>%
    group_by(zone) %>%
    summarise(total_count = n())
  
  merged_data_voltage_visualization <- merge(total_counts_per_zone, total_disconnect_droptozero_counts_per_zone, by = "zone", all = TRUE) %>%
    merge(circuit_counts_for_voltage, by = "zone", all = TRUE) %>%
    mutate(Percentage = (Count_ccts / total_count) * 100)
  
  # analysing Vmin <= 180V seperately
  data_vmin_less_than_180 <- combined_data_filtered %>%
    arrange(c_id, ts)%>%
    filter(response_category == '4 Disconnect' | response_category == '3 Drop to Zero') %>%
    filter(!is.na(vmin)) %>%
    mutate(flag_vmin = ifelse(vmin <= 180, 1, 0), 
           group_id = cumsum(flag_vmin == 0)) %>%
    group_by(c_id, group_id) %>%
    mutate(consecutive_count = cumsum(flag_vmin)) %>%
    mutate(start_time = ifelse(consecutive_count == 1, ts, NA),
           end_time = ifelse(consecutive_count == max(consecutive_count) & flag_vmin != 0, ts, NA)) %>%
    fill(start_time, .direction = "down") %>%
    fill(end_time, .direction = "up") %>%
    mutate(start_end = as.POSIXct(start_time, origin = "1970-01-01",   tz = "UTC")) %>%
    mutate(end_end = as.POSIXct(end_time, origin = "1970-01-01", tz = "UTC")) %>%
    mutate(diff_time = end_time - start_time + as.numeric(duration))
  
  for_10s_df <- data_vmin_less_than_180 %>%
    filter(diff_time == 10) %>%
    distinct(c_id, zone)
  
  less_than_10s_df <- data_vmin_less_than_180 %>%
    filter(diff_time < 10) %>%
    distinct(c_id, zone) %>%
    anti_join(for_10s_df, by = c("c_id", "zone"))
  
  more_than_10s_df <- data_vmin_less_than_180 %>%
    filter(diff_time > 10) %>%
    distinct(c_id, zone) %>%
    anti_join(for_10s_df, by = c("c_id", "zone")) %>%
    anti_join(less_than_10s_df, by = c("c_id", "zone"))
  
  combined_unique_cids <- bind_rows(
    for_10s_df %>% mutate(Voltage_Category = "Vmin < 180V for 10s"),
    less_than_10s_df %>% mutate(Voltage_Category = "Vmin < 180V for less than 10s"),
    more_than_10s_df %>% mutate(Voltage_Category = "Vmin < 180V for more than 10s")
  )
  
  data_representing_time_delay <- combined_unique_cids %>%
    group_by(Voltage_Category, zone) %>%
    summarise(Count_ccts = n_distinct(c_id))
  
  data_representing_time_delay <- merge(total_counts_per_zone, data_representing_time_delay, by = "zone", all = TRUE)
  data_representing_time_delay$Percentage <- (data_representing_time_delay$Count_ccts / data_representing_time_delay$total_count) * 100
  
  data_to_plot <- bind_rows(
    select(merged_data_voltage_visualization, zone, Voltage_Category, Percentage),
    select(data_representing_time_delay, zone, Voltage_Category, Percentage)
  )
  
  return(data_to_plot)
}
