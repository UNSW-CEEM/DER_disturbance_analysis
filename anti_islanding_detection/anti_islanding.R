# - AS4777.3 2005 5.3
VMIN_LOWER_2005 <- 200
VMIN_UPPER_2005 <- 230
VMAX_UPPER_2005 <- 270
VMAX_LOWER_2005 <- 230

# - AS4777.2 2015 table 13
UNDERVOLTAGE_2015 <- 180
OVERVOLTAGE_1_2015 <- 260
OVERVOLTAGE_2_2015 <- 265

# - AS4777.2 2020 table 4.1 
UNDERVOLTAGE_2_2020 <- 70
UNDERVOLTAGE_1_2020 <- 180
OVERVOLTAGE_1_2020 <- 265
OVERVOLTAGE_2_2020 <- 275

#' Run anti-islanding voltage threshold checks
detect_voltage_threshold_excursions <- function(combined_data) {
    combined_data <- arrange(combined_data, c_id, ts)
    
    # substitute vmin/vmax if they are NA
    combined_data <- mutate(combined_data, vmin_na=is.na(vmin))
    combined_data <- mutate(combined_data, vmax_na=is.na(vmax))
    combined_data <- mutate(combined_data, vmean_na=is.na(vmean))
    combined_data <- mutate(combined_data, vmin=ifelse(vmin_na, v, vmin))
    combined_data <- mutate(combined_data, vmax=ifelse(vmax_na, v, vmax))
    combined_data <- mutate(combined_data, vmean=ifelse(vmean_na, v, vmean))

    combined_data <- mutate(combined_data, antiislanding_v_excursion_2015=ifelse(
        vmin < UNDERVOLTAGE_2015, "undervoltage", NA))
    combined_data <- mutate(combined_data, antiislanding_v_excursion_2015=ifelse(
        vmax > OVERVOLTAGE_1_2015, "overvoltage_1", antiislanding_v_excursion_2015))
    combined_data <- mutate(combined_data, antiislanding_v_excursion_2015=ifelse(
        vmax > OVERVOLTAGE_2_2015, "overvoltage_2", antiislanding_v_excursion_2015))
    
    # combined_data <- mutate(combined_data, antiislanding_v_excursion_2020=ifelse(
    #     vmin < UNDERVOLTAGE_1_2020, "undervoltage_1", NA))
    # combined_data <- mutate(combined_data, antiislanding_v_excursion_2020=ifelse(
    #     vmin < UNDERVOLTAGE_2_2020, "undervoltage_2", antiislanding_v_excursion_2020))
    # combined_data <- mutate(combined_data, antiislanding_v_excursion_2020=ifelse(
    #     vmax > OVERVOLTAGE_1_2020, "overvoltage_1", antiislanding_v_excursion_2020))
    # combined_data <- mutate(combined_data, antiislanding_v_excursion_2020=ifelse(
    #     vmax > OVERVOLTAGE_2_2020, "overvoltage_2", antiislanding_v_excursion_2020))
    
    return(combined_data)
}

antiislanding_summary <- function(combined_data) {
    combined_data <- mutate(combined_data, is_initial=(
        (antiislanding_v_excursion_2015 != lag(antiislanding_v_excursion_2015)) |
            (!is.na(antiislanding_v_excursion_2015) & is.na(lag(antiislanding_v_excursion_2015)))
    ))
    combined_data <- mutate(combined_data, is_final=(
        (antiislanding_v_excursion_2015 != lead(antiislanding_v_excursion_2015)) |
        (!is.na(antiislanding_v_excursion_2015) & is.na(lead(antiislanding_v_excursion_2015)))
    ))
    combined_data <- ungroup(group_by(combined_data, va_event_id=cumsum(!is.na(is_initial) & is_initial)))
    combined_data <- mutate(combined_data, va_event_id=ifelse(is.na(antiislanding_v_excursion_2015), NA, va_event_id))
    combined_data <- mutate(combined_data, va_2015_recurrances=sequence(rle(antiislanding_v_excursion_2015)$length))
    voltage_summary <- combined_data %>%
        group_by(va_event_id) %>%
        summarise(c_id=first(c_id), v=mean(v), vmin=min(vmin), vmax=max(vmax), vmean=mean(vmean), d=last(d),
                  antiislanding_v_excursion_2015=first(antiislanding_v_excursion_2015),
                  va_2015_recurrances=max(va_2015_recurrances), Standard_Version=first(Standard_Version)
                  )
    return(voltage_summary)
}

summarise_voltage_data <- function(combined_data) {
    summarised_voltage_data <- combined_data %>%
        group_by(c_id) %>%
        summarise(
            vmax_max=max(vmax), vmin_min=min(vmin), vmean_mean=mean(vmean),
            vmin_na_all=all(vmin_na), vmax_na_all=all(vmax_na), vmean_na_all=all(vmean_na),
            antiislanding_v_excursion_2015_triggered=any(!is.na(antiislanding_v_excursion_2015))
        )
    summarised_voltage_data <- select(
        summarised_voltage_data, c_id, vmax_max, vmin_min, vmean_mean,
        vmin_na_all, vmax_na_all, vmean_na_all,
        antiislanding_v_excursion_2015_triggered)
    return(summarised_voltage_data)
}
