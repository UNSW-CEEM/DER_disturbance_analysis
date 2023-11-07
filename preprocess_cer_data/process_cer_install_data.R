library(data.table)

install_data_file <- "cer_data/cer_sizegroup_install_data.csv"
install_data <- read.csv(file = install_data_file, header = TRUE, stringsAsFactors = FALSE)

start_date <- min(install_data$date)
end_date <- max(install_data$date)
date_vector <- seq(as.Date(start_date), as.Date(end_date), by = "days")

type_table <- data.table(
  state = c("QLD", "NSW", "VIC", "TAS", "SA", "WA", "QLD", "NSW", "VIC", "TAS", "SA", "WA"),
  sizegroup = c(
    "<30 kW",
    "<30 kW",
    "<30 kW",
    "<30 kW",
    "<30 kW",
    "<30 kW",
    "30-100kW",
    "30-100kW",
    "30-100kW",
    "30-100kW",
    "30-100kW",
    "30-100kW"
  ),
  dummy = 1
)

type_table <- setkey(type_table, dummy)

date_table <- data.table(date = date_vector, dummy = 1)
date_table <- date_table[, date := as.Date(date)]
date_table <- setkey(date_table, dummy)

date_table <- date_table[type_table, allow.cartesian = TRUE]
date_table <- setkey(date_table, state, sizegroup, date)

install_data <- mutate(install_data)
install_data <- data.table(install_data)
install_data <- install_data[, date := as.Date(date)]
install_data <- setkey(install_data, state, sizegroup, date)

combined <- as.data.frame(install_data[date_table, roll = T])

combined[is.na(combined)] <- 0

combined <- select(combined, state, sizegroup, date, number, capacity)

write.csv(combined, "cer_data/cer_sizegroup_install_data_no_gaps.csv", row.names = FALSE)
