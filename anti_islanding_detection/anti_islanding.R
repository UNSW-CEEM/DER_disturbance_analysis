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
    voltage_data <- select(combined_data, ts, c_id, Standard_Version, v, vmin, vmax, vmean, d)
    voltage_data <- arrange(voltage_data, c_id, ts)
    voltage_data <- mutate(voltage_data, vmin_exists=!is.na(vmin))
    
    # substitute vmin/vmax if they are NA
    voltage_data <- mutate(voltage_data, vmin=ifelse(is.na(vmin), v, vmin))
    voltage_data <- mutate(voltage_data, vmax=ifelse(is.na(vmax), v, vmax))
    voltage_data <- mutate(voltage_data, vmean=ifelse(is.na(vmean), v, vmean))

    voltage_data <- mutate(voltage_data, voltage_antiislanding_2015=ifelse(
        vmax < UNDERVOLTAGE_2015, "undervoltage", NA))
    voltage_data <- mutate(voltage_data, voltage_antiislanding_2015=ifelse(
        vmin > OVERVOLTAGE_1_2015, "overvoltage_1", voltage_antiislanding_2015))
    voltage_data <- mutate(voltage_data, voltage_antiislanding_2015=ifelse(
        vmin > OVERVOLTAGE_2_2015, "overvoltage_2", voltage_antiislanding_2015))
    voltage_data <- mutate(voltage_data, va_2015_recurrances=sequence(rle(voltage_antiislanding_2015)$length))

    return(voltage_data)
}


antiislanding_summary <- function(voltage_data) {
    voltage_data <- mutate(voltage_data, is_initial=(
        (voltage_antiislanding_2015 != lag(voltage_antiislanding_2015)) |
            (!is.na(voltage_antiislanding_2015) & is.na(lag(voltage_antiislanding_2015)))
    ))
    voltage_data <- mutate(voltage_data, is_final=(
        (voltage_antiislanding_2015 != lead(voltage_antiislanding_2015)) |
        (!is.na(voltage_antiislanding_2015) & is.na(lead(voltage_antiislanding_2015)))
    ))
    voltage_data <- ungroup(group_by(voltage_data, va_event_id=cumsum(!is.na(is_initial) & is_initial)))
    voltage_data <- mutate(voltage_data, va_event_id=ifelse(is.na(voltage_antiislanding_2015), NA, va_event_id))
    voltage_summary <- voltage_data %>%
        group_by(va_event_id) %>%
        drop_na() %>%
        summarise(c_id=first(c_id), v=mean(v), vmin=min(vmin), vmax=max(vmax), vmean=mean(vmean), d=last(d),
                  voltage_antiislanding_2015=first(voltage_antiislanding_2015),
                  va_2015_recurrances=max(va_2015_recurrances), Standard_Version=first(Standard_Version)
                  )
    return(voltage_summary)
}

summarise_voltage_data <- function(voltage_data) {
    summarised_voltage_data <- voltage_data %>%
        group_by(c_id) %>%
        summarise(
            vmax=max(vmax), vmin=min(vmin), vmean=mean(vmean),
            vmin_exists=all(vmin_exists),
            voltage_antiislanding_2015=any(!is.na(voltage_antiislanding_2015))
        )
    summarised_voltage_data <- select(
        summarised_voltage_data, c_id, vmax, vmin, vmean, voltage_antiislanding_2015)
    return(summarised_voltage_data)
}
