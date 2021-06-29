source("load_tool_environment.R")

db <- DBInterface$new()

db$connect_to_existing_database("data/2021-05-25/20210525.db")

ufls_statuses <- ufls_detection(db = db, region = 'QLD', 
                                pre_event_interval = strptime('2021-05-25 14:06:40', format="%Y-%m-%d %H:%M:%S", 
                                                              tz="Australia/Brisbane"), 
                                event_window_length = 5, pct_sample_reduction_threshold = 0.5)

write.csv(ufls_statuses, as.character("data/2021-05-25/ufls_statuses.csv"), row.names=FALSE)