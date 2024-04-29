process_time_series_data <- function(time_series_data) {
  time_series_data <- time_series_data %>%
    mutate(ts = fastPOSIXct(ts, tz = "Australia/Brisbane")) %>%
    mutate(e = as.numeric(e)) %>%
    mutate(v = as.numeric(v)) %>%
    mutate(f = as.numeric(f))
  return(time_series_data)
}

get_time_offsets <- function(time_series_data) {
  time_series_data <- mutate(time_series_data, time_offset = as.numeric(format(ts, "%S")) %% d)
  return(time_series_data)
}

get_time_series_unique_offsets <- function(time_series_data) {
  unique_offsets <- unique(time_series_data$time_offset)
  return(unique_offsets)
}

make_offset_filter_label <- function(sample_counts, unique_offsets) {
  label <- "Select time offset data subset to use, ("
  for(i in 1:length(unique_offsets)) {
    label <- paste0(label, unique_offsets[i], ": n=", sample_counts[i], ", ")
  }
  label <- paste0(label, ")")
  return(label)
}

get_offset_sample_counts <- function(time_series_data, unique_offsets) {
  time_series_data <- distinct(time_series_data, c_id, .keep_all = TRUE)
  sample_counts <- c()
  for(i in 1:length(unique_offsets)) {
    sample_counts <- c(sample_counts, length(filter(time_series_data, time_offset == unique_offsets[i])$c_id))
  }
  return(sample_counts)
}


process_raw_site_details <- function(site_details) {
  site_details <- filter(site_details, !is.na(ac) & ac != "")
  assert_raw_site_details_assumptions(site_details)
  # The data can contain duplicate site IDs. these need to be sumarised so there
  # is one row per site ID. AC power is summed so sites with more than 100kW
  # can be filtered out of the dataset. The first AC value is taken as a sample
  # of the site, it is not summed, as later when the data is joined to the
  # circuit data (which may have mutiple rows be site_id) this would create
  # apparent additional AC capacity.
  processed_site_details <- site_details %>%
    group_by(site_id) %>%
    summarise(
      s_state = first(s_state),
      pv_installation_year_month = first(pv_installation_year_month),
      sum_ac = sum(ac),
      first_ac = first(ac),
      s_postcode = first(s_postcode),
      manufacturer = paste(manufacturer, collapse = " "),
      model = paste(model, collapse = " ")
    ) %>%
    as.data.frame()
  return(processed_site_details)
}

sum_manufacturers <- function(manufacturers) {
  unique_manufacturers <- unique(manufacturers)
  if (anyNA(unique_manufacturers)) {
    manufacturer <- "NA"
  } else if (length(unique_manufacturers) > 1) {
    manufacturer <- "Multiple"
  } else {
    manufacturer <- unique_manufacturers[1]
  }
  return(manufacturer)
}

#' Check incoming site data for conformance to data processing assumptions.
assert_raw_site_details_assumptions <- function(site_details) {
  # Only possible s_state values are NSW, QLD, VIC, TAS, SA, WA, NT, ACT.
  s_state <- site_details$s_state
  assert_that(
    all(s_state %in% c("NSW", "QLD", "VIC", "TAS", "SA" , "WA", "ACT")),
    msg = "State values outside expected set NSW, ACT, SA etc."
  )
  # Only one distinct s_state and s_postcode value for each site_id.
  site_details_grouped <- group_by(site_details, site_id) %>%
    reframe(s_state = unique(s_state), s_postcode = unique(s_postcode))
  assert_that(
    all(count(site_details_grouped, site_id, s_state)$n == 1),
    msg = "Some sites have mutiple distinct s_state values."
  )
  assert_that(
    all(count(site_details_grouped, site_id, s_postcode)$n == 1),
    msg = "Some sites have mutiple distinct s_postcode values."
  )
  # Assume AC values can be converted to numeric without be turned into NAs.
  assert_that(all(!is.na(as.numeric(site_details$ac))))
}

perform_power_calculations <- function(master_data_table) {
  # Calculate the average power output over the sample time base on the
  # cumulative energy and duration length.Assuming energy is Joules and duration is in seconds.
  master_data_table <- mutate(master_data_table, e_polarity = e * polarity)
  master_data_table <- mutate(master_data_table, power_kW = e_polarity / (d * 1000))
  return(master_data_table)
}

process_install_data <- function(install_data) {
  assert_install_data_assumptions(install_data)
  # Rename time column and categorise data based on inverter standards.
  install_data <- setnames(
    install_data,
    c("state", "sizegroup", "date", "capacity"),
    c("s_state", "Grouping", "pv_installation_year_month", "Capacity")
  )
  install_data <- site_categorisation(install_data)
  # Convert column names to same format as Solar Analytics data.
  install_data <- setnames(install_data, c("pv_installation_year_month"), c("date"))
  # For each inverter standard group find the install capacity when the standard came into force.
  start_date <- min(install_data$date)
  installed_start_standard <- install_data %>%
    group_by(Standard_Version, Grouping, s_state) %>%
    summarise(
      initial_cap = install_data$Capacity[
        max(
          c(
            install_data$date[
              (install_data$Capacity < min(Capacity)) &
              (install_data$s_state == first(s_state)) &
              (install_data$Grouping == first(Grouping))
            ],
            start_date
          )
        ) == install_data$date &
        install_data$s_state == first(s_state) &
        install_data$Grouping == first(Grouping)
      ]
    ) %>%
    as.data.frame() %>%
    mutate(initial_cap = ifelse(Standard_Version == "AS4777.3:2005", 0, initial_cap))
  # Join the initial capacity to the cumulative capacity table.
  install_data <- inner_join(install_data, installed_start_standard, by = c("Standard_Version", "Grouping", "s_state"))
  # Calculate installed capacity by standard.
  install_data <- mutate(install_data, standard_capacity = Capacity - initial_cap)
  return(install_data)
}

assert_install_data_assumptions <- function(install_data) {
  # Assert that the date column is convertable to a date object using the assumed format.
  assert_that(
    all(!is.na(as.Date(install_data$date, format = "%Y-%m-%d"))),
    msg = "pv_installation_year_month has an unexpected format, should be YYYY-MM-DD"
  )
  # Assert groupings only one of two options
  assert_that(
    all(install_data$sizegroup %in% c("30-100kW" ,"<30 kW")),
    msg = "Grouping values in install data do not match the expected values"
  )
  # Assert that all capacity values can be converted to numeric without creating nas.
  assert_that(
    suppressWarnings(all(!is.na(as.numeric(install_data$capacity)))),
    msg = "Not all capacity values can convert to numeric in install data"
  )
  # Assert that State values are within expected set.
  assert_that(
    all(install_data$state %in% c("NSW", "VIC", "SA", "TAS", "QLD", "NT", "ACT", "WA")),
    msg = "State values in install data do not match the expected values"
  )
  # Assert that the first date in the install data is before the start of the transition period
  assert_that(
    min(ymd(install_data$date)) < ymd("2015-10-01"),
    msg = "Install data first entry does not predate start of transition period"
  )
}

size_grouping <- function(site_details) {
  # Categorise site by sample AC capacity.
  site_details <- mutate(site_details, Grouping = ifelse(first_ac >= 30, "30-100kW" ,"<30 kW"))
  return(site_details)
}

process_postcode_data <- function(postcode_data) {
  assert_postcode_data_assumptions(postcode_data)
  postcode_data <- mutate(postcode_data, postcode = as.integer(postcode))
  postcode_data <- filter(postcode_data, !is.na(lat) & !is.na(lon))
  return(postcode_data)
}

assert_postcode_data_assumptions <- function(postcode_data) {
  # We assume that all values in the lat column can be safely converted to numeric type
  assert_that(
    suppressWarnings(all(!is.na(as.numeric(postcode_data$lat)))),
    msg = "Not all lat values could be interpreted as numbers"
  )
  # We assume that all values in the lon column can be safely converted to numeric type
  assert_that(
    suppressWarnings(all(!is.na(as.numeric(postcode_data$lon)))),
    msg = "Not all lat values could be interpreted as numbers"
  )
  # We assume that all values in the s_postcode column can be safely converted to numeric type
  assert_that(
    suppressWarnings(all(!is.na(as.numeric(postcode_data$postcode)))),
    msg = "Not all s_postcode values could be interpreted as numbers"
  )
}

site_categorisation <- function(combined_data) {
  # Processes installed month. Setting missing month values to jan 2005, and using assumed day of month as the 28th.
  # Then categorising into standard version based on date.
  combined_data <- combined_data %>%
    mutate(
      pv_installation_year_month = ifelse(pv_installation_year_month == "", "2015-11", pv_installation_year_month)
    ) %>%
    mutate(
      pv_installation_year_month = ifelse(pv_installation_year_month == "NaT", "2015-11", pv_installation_year_month)
    ) %>%
    mutate(
      pv_installation_year_month = ifelse(
        pv_installation_year_month == "0/01/1900",
        "2015-11",
        pv_installation_year_month)
    )
  column_index <- grep("^pv_installation_year_month$", colnames(combined_data))
  set(combined_data, which(is.na(combined_data[[column_index]])), column_index, "2015-11")
  combined_data <- combined_data %>%
    mutate(
      pv_installation_year_month = ifelse(
        nchar(pv_installation_year_month) == 10,
        pv_installation_year_month,
        paste0(pv_installation_year_month, "-28")
      )
    ) %>%
    mutate(pv_installation_year_month = ymd(pv_installation_year_month)) %>%
    mutate(Standard_Version = ifelse(pv_installation_year_month < "2016-11-01", "AS4777.3:2005", "")) %>%
    # Assumes that systems installed from Nov 2016 to Mar 2023 are "AS4777.2:2015"
    # Note that 2020 std was made compulsory in Dec 2022 but had low compliance in first year.
    mutate(
      Standard_Version = ifelse(
        pv_installation_year_month >= "2016-11-01" & pv_installation_year_month < "2023-04-01",
        "AS4777.2:2015",
        Standard_Version
      )
    ) %>%
    # Assumes systems installed from April 2023 are "AS4777.2:2020"
    mutate(
      Standard_Version = ifelse(
        pv_installation_year_month >= "2023-04-01",
        "AS4777.2:2020 (>=Apr'23)",
        Standard_Version
      )
    )

  return(combined_data)
}

assert_site_install_date_assumptions <- function(site_details) {
  # Check incoming data for conformance to data processing assumptions.
  # We assume that the PV installation year month will be either of the form YYYY-MM or YYYY-MM-DD
  assert_that(
    all(length(pv_installation_year_month) %in% c(7, 10)),
    msg = "pv_installation_year_month has an unexpected format, should be YYYY-MM or YYYY-MM-DD"
  )
}

combine_data_tables <- function(time_series_data, circuit_details, site_details) {
  circuit_details <- process_raw_circuit_details(circuit_details)
  site_details <- site_categorisation(site_details)
  site_details <- size_grouping(site_details)
  time_series_data <- inner_join(time_series_data, circuit_details, by = c("c_id"))
  time_series_data <- inner_join(time_series_data, site_details, by = c("site_id"))
  time_series_data <- perform_power_calculations(time_series_data)
  return(time_series_data)
}

get_mode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
