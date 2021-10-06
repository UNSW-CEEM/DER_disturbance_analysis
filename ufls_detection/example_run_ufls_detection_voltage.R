source("load_tool_environment.R")

db_file <- "20210525_Tesla.db"

ts <- sqldf::sqldf("select * from timeseries", dbname = db_file)

ufls_statuses_v <- ufls_detection_voltage(combined_data = ts, 
    event_time = strptime('2021-05-25 04:06:00', format="%Y-%m-%d %H:%M:%S", 
                          tz="Australia/Brisbane"), 
    window_length = 5, 
    fill_nans = TRUE)

write.csv(ufls_statuses_v, as.character("data/Tesla_ufls.csv"), row.names=FALSE)