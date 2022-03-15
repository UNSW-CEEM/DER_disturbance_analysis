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

detect_voltage_thresholds <- function(combined_data) {
    voltage_data <- select(combined_data, ts, c_id, Standard_Version, v, vmin, vmax, d)
    voltage_data <- arrange(voltage_data, c_id, ts)
    # substitute vmin/vmax if they are NA
    voltage_data <- mutate(voltage_data, vmin=ifelse(is.na(vmin), v, vmin))
    voltage_data <- mutate(voltage_data, vmax=ifelse(is.na(vmax), v, vmax))

    voltage_data <- mutate(voltage_data, voltage_antiislanding_2015=ifelse(
        vmin < UNDERVOLTAGE_2015, "undervoltage", NA))
    voltage_data <- mutate(voltage_data, voltage_antiislanding_2015=ifelse(
        vmax > OVERVOLTAGE_1_2015, "overvoltage_1", voltage_antiislanding_2015))
    voltage_data <- mutate(voltage_data, voltage_antiislanding_2015=ifelse(
        vmax > OVERVOLTAGE_2_2015, "overvoltage_2", voltage_antiislanding_2015))
    voltage_data <- mutate(voltage_data, va_2015_recurrances=sequence(rle(voltage_antiislanding_2015)$length))
    voltage_data <- mutate(voltage_data, va_2015_duration_upper=ifelse(
        va_2015_recurrances > 1, (va_2015_recurrances-1)*d+d*2, NA))

    return(voltage_data)
}