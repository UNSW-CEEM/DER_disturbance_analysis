source("load_tool_environment.R")

db <- DBInterface$new()

db$connect_to_existing_database("Data/2021-05-25 QLD/20210525_QLD_database_new.db")

ufls_statuses_ts <- ufls_detection_tstamp(
  db = db,
  region = "NSW",
  pre_event_interval = strptime("2021-05-25 14:06:40", format = "%Y-%m-%d %H:%M:%S", tz = "Australia/Brisbane"),
  pre_event_window_length = 5,
  post_event_window_length = 9,
  pre_pct_sample_seconds_threshold = 0.6
)

write.csv(ufls_statuses_ts, as.character("Data/2021-05-25 QLD/1406/NSW_new_criteria_UFLS.csv"), row.names = FALSE)
