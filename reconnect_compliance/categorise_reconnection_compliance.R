categorise_row <- function(response_category,
                           ramp_above_threshold,
                           ramp_threshold_for_compliance,
                           ramp_threshold_for_non_compliance) {
  if (response_category %in% c("4 Disconnect", "3 Drop to Zero")) {
    if (is.na(ramp_above_threshold)) {
      category <- "Cannot be set"
    } else {
      if (ramp_above_threshold < ramp_threshold_for_compliance) {
        category <- "Compliant"
      } else if (ramp_above_threshold > ramp_threshold_for_non_compliance) {
        category <- "Non Compliant"
      } else {
        category <- "Unsure"
      }
    }
  } else {
    category <- NA
  }
  return(category)
}

categorise_reconnection_compliance <- function(ramping_data,
                                               ramp_threshold_for_compliance,
                                               ramp_threshold_for_non_compliance) {
  ramping_data <- rowwise(ramping_data) %>%
    mutate(
      reconnection_compliance_status = categorise_row(
        response_category,
        ramp_above_threshold,
        ramp_threshold_for_compliance,
        ramp_threshold_for_non_compliance
      )
    ) %>%
    select(c_id, reconnection_compliance_status)
  return(ramping_data)
}
