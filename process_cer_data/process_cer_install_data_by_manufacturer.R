library(data.table)
library(dplyr)

intall_data_file <- "C:/Users/NGorman/Documents/cer_installs_by_manufacturer.csv"
install_data <- read.csv(file=intall_data_file, header=TRUE, stringsAsFactors = FALSE)

start_date = min(install_data$index)
end_date = max(install_data$index)
date_vector <- seq(as.Date(start_date), as.Date(end_date), by="days")

manufcaturers <- unique(install_data$manufacturer)

state_table <- data.table(
  State = c('QLD', 'NSW', "VIC", "TAS", 'SA', 'WA'),
  dummy = 1
)

manufacturer_table <- data.table(
  manufacturer = manufcaturers,
  dummy = 1
)

state_table <- setkey(state_table, dummy)
manufacturer_table <- setkey(manufacturer_table, dummy)
type_table <- state_table[manufacturer_table, allow.cartesian=TRUE]

date_table <- data.table(date = date_vector, dummy = 1)
date_table <- date_table[, date:=as.Date(date)]
date_table <- setkey(date_table, dummy)

date_table <- date_table[type_table, allow.cartesian=TRUE]
date_table <- setkey(date_table, State, manufacturer, date)

install_data <- mutate(install_data)
install_data <- data.table(install_data)
install_data <- install_data[, index := as.Date(index)]
install_data <- setkey(install_data, State, manufacturer, index)

combined <- as.data.frame(install_data[date_table, roll = T ])

combined <- filter(combined, format(index, format = '%d') == "01")

combined <- select(combined, index, State, manufacturer, Capacity, Number)

combined[is.na(combined)] <- 0

write.csv(combined, "pv_installed_by_manufacturer_formatted.csv", row.names=FALSE)

#intall_data_file <- "GitHub/DER_disturbance_analysis/cumulative_capacity_and_number_20200218_new.csv"
#install_data <- read.csv(file=intall_data_file, header=TRUE, stringsAsFactors = FALSE)