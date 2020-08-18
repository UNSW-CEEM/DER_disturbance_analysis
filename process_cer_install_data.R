library(data.table)

intall_data_file <- "GitHub/DER_disturbance_analysis/cumulative_capacity_and_number_20200811_raw_from_sql_v2.csv"
install_data <- read.csv(file=intall_data_file, header=TRUE, stringsAsFactors = FALSE)

start_date =min(install_data$index)
end_date =max(install_data$index)
date_vector <- seq(as.Date(start_date), as.Date(end_date), by="days")

type_table <- data.table(
  State = c('QLD', 'NSW', "VIC", "TAS", 'SA', 'WA', 'QLD', 'NSW', "VIC", "TAS", 'SA', 'WA'),
  Grouping =c('<30 kW', '<30 kW','<30 kW','<30 kW','<30 kW', '<30 kW', 
              '30-100kW', '30-100kW', '30-100kW', '30-100kW', '30-100kW', '30-100kW'),
  dummy = 1
)

type_table <- setkey(type_table, dummy)

date_table <- data.table(date = date_vector, dummy = 1)
date_table <- date_table[, date:=as.Date(date)]
date_table <- setkey(date_table, dummy)

date_table <- date_table[type_table, allow.cartesian=TRUE]
date_table <- setkey(date_table, State, Grouping, date)

install_data <- mutate(install_data)
install_data <- data.table(install_data)
install_data <- install_data[, index:=as.Date(index)]
install_data <- setkey(install_data, State, Grouping, index)

combined <- as.data.frame(install_data[date_table, roll = T ])

combined[is.na(combined)] <- 0

write.csv(combined, "GitHub/DER_disturbance_analysis/cumulative_capacity_and_number_20200811_ready_for_tool_v2.csv", row.names=FALSE)

#intall_data_file <- "GitHub/DER_disturbance_analysis/cumulative_capacity_and_number_20200218_new.csv"
#install_data <- read.csv(file=intall_data_file, header=TRUE, stringsAsFactors = FALSE)