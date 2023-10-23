library(data.table)
library(dplyr)

intall_data_file <- "cer_data/cer_test_data.csv"
install_data <- read.csv(file = intall_data_file, header = TRUE, stringsAsFactors = FALSE)

start_date = min(install_data$date)
end_date = max(install_data$date)
date_vector <- seq(as.Date(start_date), as.Date(end_date), by = "days")

manufacturers <- unique(install_data$manufacturer)

state_table <- data.table(
  state = c("QLD", "NSW", "VIC", "TAS", "SA", "WA"),
  dummy = 1
)

manufacturer_table <- data.table(
  manufacturer = manufacturers,
  dummy = 1
)

state_table <- setkey(state_table, dummy)
manufacturer_table <- setkey(manufacturer_table, dummy)
type_table <- state_table[manufacturer_table, allow.cartesian = TRUE]

date_table <- data.table(date = date_vector, dummy = 1)
date_table <- date_table[, date = as.Date(date)]
date_table <- setkey(date_table, dummy)

date_table <- date_table[type_table, allow.cartesian = TRUE]
date_table <- setkey(date_table, state, manufacturer, date)

install_data <- mutate(install_data)
install_data <- data.table(install_data)
install_data <- install_data[, date = as.Date(date)]
install_data <- setkey(install_data, state, manufacturer, date)

combined <- as.data.frame(install_data[date_table, roll = T])

combined <- filter(combined, format(date, format = "%d") == "01")

combined <- select(combined, date, state, manufacturer, capacity, number)

combined[is.na(combined)] <- 0

write.csv(combined, "cer_data/cer_test_data_no_gaps.csv", row.names = FALSE)
