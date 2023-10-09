calc_confidence_intervals_for_disconnections <- function(disconnection_summary) {
  result <- mapply(confidence_interval, disconnection_summary$total, disconnection_summary$disconnections, 0.95)
  disconnection_summary$lower <- result[1,]
  disconnection_summary$upper <- result[2,]
  disconnection_summary <- mutate(
    disconnection_summary,
    lower_error = proportion - lower,
    upper_error = upper - proportion
  )
  return(disconnection_summary)
}
