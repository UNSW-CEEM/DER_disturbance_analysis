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
#' If multiple levels occur in the same interval (hopefully unlikely) the higher
#' voltage event currently takes precedence.
detect_voltage_threshold_excursions <- function(combined_data, pre_event_interval, window_length) {
  combined_data <- combined_data %>%
    mutate(in_event_window = ts > pre_event_interval & ts <= pre_event_interval + 60 * window_length) %>%
    # substitute vmin/vmax if they are NA
    mutate(vmin_na = is.na(vmin)) %>%
    mutate(vmax_na = is.na(vmax)) %>%
    mutate(vmean_na = is.na(vmean)) %>%
    mutate(vmin = ifelse(vmin_na, v, vmin)) %>%
    mutate(vmax = ifelse(vmax_na, v, vmax)) %>%
    mutate(vmean = ifelse(vmean_na, v, vmean)) %>%
    mutate(
      antiislanding_v_excursion_2015 = ifelse((vmin < UNDERVOLTAGE_2015) & in_event_window, "undervoltage", NA)
    ) %>%
    mutate(
      antiislanding_v_excursion_2015 = ifelse(
        (vmax > OVERVOLTAGE_1_2015) & in_event_window,
        "overvoltage_1",
        antiislanding_v_excursion_2015
      )
    ) %>%
    mutate(
      antiislanding_v_excursion_2015 = ifelse(
        (vmax > OVERVOLTAGE_2_2015) & in_event_window,
        "overvoltage_2",
        antiislanding_v_excursion_2015
      )
    ) %>%
    mutate(
      antiislanding_v_excursion_2020 = ifelse((vmin < UNDERVOLTAGE_1_2020) & in_event_window, "undervoltage_1", NA)
    ) %>%
    mutate(
      antiislanding_v_excursion_2020 = ifelse(
        (vmin < UNDERVOLTAGE_2_2020) & in_event_window,
        "undervoltage_2",
        antiislanding_v_excursion_2020
      )
    ) %>%
    mutate(
      antiislanding_v_excursion_2020 = ifelse(
        (vmax > OVERVOLTAGE_1_2020) & in_event_window,
        "overvoltage_1",
        antiislanding_v_excursion_2020
      )
    ) %>%
    mutate(
      antiislanding_v_excursion_2020 = ifelse(
        (vmax > OVERVOLTAGE_2_2020) & in_event_window,
        "overvoltage_2",
        antiislanding_v_excursion_2020
      )
    )

  return(combined_data)
}

#' Find the number of consecutive occurrences of each antiislanding threshold crossing
#' @param antiislanding_column: the column containing which threshold has been crossed
#' @param recurrences_columm: the name of the column for the output count
#' Note: this function is horrible to read thanks to a bunch of dereferencing(?)/symbol conversion
#'       required to get use variable column names with DPLYR. Some of the sym() calls may be superfluous
#'       but I'm not going to fix them yet. Make changes at your own risk!
summarise_antiislanding_recurrences <- function(combined_data, antiislanding_column, recurrences_column) {
  combined_data <- arrange(combined_data, c_id, ts)
  id_column <- paste0(recurrences_column, "_id")
  combined_data <- combined_data %>%
    mutate(
      is_initial = (
        (!!sym(antiislanding_column) != lag(!!sym(antiislanding_column))) |
        (!is.na(!!sym(antiislanding_column)) & is.na(lag(!!sym(antiislanding_column))))
      )
    ) %>%
    group_by(!!sym(id_column) := cumsum(!is.na(is_initial) & is_initial)) %>%
    ungroup() %>%
    mutate(!!id_column := ifelse(is.na(!!sym(antiislanding_column)), NA, !!sym(id_column))) %>%
    mutate(!!recurrences_column := sequence(rle(!!sym(antiislanding_column))$length))

  summary <- combined_data %>%
    group_by(!!sym(id_column)) %>%
    summarise(
      c_id = first(c_id),
      v = mean(v),
      vmin = min(vmin),
      vmax = max(vmax),
      vmean = mean(vmean),
      d = last(d),
      !!antiislanding_column := first(!!sym(antiislanding_column)),
      !!recurrences_column := max(!!sym(recurrences_column)),
      Standard_Version = first(Standard_Version),
      start_tstamp = min(ts),
      end_tstamp = max(ts)
    ) %>%
    filter(!is.na(!!sym(id_column)))
  return(summary)
}

antiislanding_summary <- function(combined_data) {
  voltage_summary_2015 <- summarise_antiislanding_recurrences(
    combined_data,
    "antiislanding_v_excursion_2015",
    "va_2015_recurrences"
  )
  voltage_summary_2020 <- summarise_antiislanding_recurrences(
    combined_data,
    "antiislanding_v_excursion_2020",
    "va_2020_recurrences"
  )
  voltage_summary <- bind_rows(voltage_summary_2015, voltage_summary_2020)
  return(voltage_summary_2015)
}

summarise_voltage_data <- function(combined_data) {
  summarised_voltage_data <- combined_data %>%
    filter(in_event_window) %>%
    group_by(c_id) %>%
    summarise(
      vmax_max = max_with_missing(ifelse(vmax_na, v, vmax)),
      vmin_min = min_with_missing(ifelse(vmin_na, v, vmin)),
      vmean_mean = mean(ifelse(vmean_na, v, vmean), na.rm = TRUE),
      vmin_na_all = all(vmin_na),
      vmax_na_all = all(vmax_na),
      vmean_na_all = all(vmean_na),
      antiislanding_v_excursion_2015_triggered = any(!is.na(antiislanding_v_excursion_2015)),
      antiislanding_v_excursion_2020_triggered = any(!is.na(antiislanding_v_excursion_2020))
    ) %>%
    mutate(
      voltage_excursion = case_when(
        antiislanding_v_excursion_2015_triggered & antiislanding_v_excursion_2020_triggered ~ "V Excursion 2015 & 2020",
        antiislanding_v_excursion_2015_triggered ~ "V Excursion 2015",
        antiislanding_v_excursion_2020_triggered ~ "V Excursion 2020",
        TRUE ~ "No V Excursion"
      )
    ) %>%
    select(
      c_id,
      vmax_max,
      vmin_min,
      vmean_mean,
      vmin_na_all,
      vmax_na_all,
      vmean_na_all,
      antiislanding_v_excursion_2015_triggered,
      antiislanding_v_excursion_2020_triggered,
      voltage_excursion
    )
  return(summarised_voltage_data)
}

max_with_missing <- function(x) ifelse(!all(is.na(x)), max(x, na.rm = TRUE), NA)

min_with_missing <- function(x) ifelse(!all(is.na(x)), min(x, na.rm = TRUE), NA)
