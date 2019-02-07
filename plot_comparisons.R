source("data_manipulation_functions.R")
source("filter_and_aggregate.R")
pdf("plots2.pdf")
# Define files to use
Fault_Datafile <- "test_data/2018-08-25 raw inputs/2018-08-25_sa_qld_naomi.feather"
Circuit_Datafile <- "test_data/2018-08-25 pre-cleaned inputs/circuit_details_TB_V4.csv"
Site_Datafile <- "test_data/2018-08-25 raw inputs/site_details.csv"

# Load Data
time_series_data <- read_feather(Fault_Datafile)
circuit_details <- read.csv(file=Circuit_Datafile, header=TRUE, stringsAsFactors = FALSE)  %>% 
  mutate(manual_check_required = ifelse(is.na(manual_check_required), 0, 1)) %>% 
  mutate(ns_change_flag = ifelse(is.na(ns_change_flag), 0, 1))
circuit_details <- filter(circuit_details, manual_check_required!=1)
circuit_details <- filter(circuit_details, ns_change_flag!=1)
site_details <- read.csv(file=Site_Datafile, header=TRUE, stringsAsFactors = FALSE)
site_details <- process_raw_site_details(site_details)

# Peform data processing
combined_data <- combine_data_tables(time_series_data, circuit_details, 
                                     site_details)

#### Compare "AS4777.3:2005"
combined_data_ignore <- filter(combined_data, pv_installation_year_month!="2020-01-28")
combined_data_f <- vector_filter(combined_data, duration=60, 
                                 state="SA", standards="AS4777.3:2005")
misfits <- filter(combined_data, pv_installation_year_month=="2020-01-28" & s_state=="SA" & d==60) 
combined_data_f <- rbind(combined_data_f, misfits)
combined_data_f_ignore <- vector_filter(combined_data, duration=60, 
                                 state="SA", standards="AS4777.3:2005")
agg_power_keep <- vector_groupby(combined_data_f, agg_on_standard=TRUE)
agg_power_ignore <- vector_groupby(combined_data_f_ignore, agg_on_standard=TRUE)

#Load test data from Naomi's work 
test_data_file <- 'test_data/2018-08-25 test output/2018-08-25 SA Agg Power by Standard.csv'
test_data <- read.csv(file=test_data_file, header=TRUE, stringsAsFactors = FALSE) %>%
  mutate(Time = ymd_hms(Time, tz="Australia/Brisbane"))

plot(NULL, ylab="Power (kW)", xlab="Time", xlim=c(min(agg_power$Time), max(agg_power$Time)), ylim=c(0, 500))
lines(agg_power_keep$Time, agg_power_keep$Power_kW, lty=2, col="red", lwd = 2)
lines(test_data$Time, test_data$standard_2005, lty=1, col="blue", lwd = 2)
lines(agg_power_ignore$Time, agg_power_ignore$Power_kW, lty=2, col="green", lwd = 2)

# Add a legend
legend(x='bottomleft', y=NULL, legend=c("R method keep No install dates", "R method ignore No install dates", "Naomi's aggregate"),
       col=c("red", "green", "blue"), lty=c(2, 2, 1), cex=0.8)
title("R method comparison for AS4777.3:2005")

#### Compare "AS4777.3:2005"
#combined_data_f <- filter(combined_data, pv_installation_year_month < "2016-10-09")
combined_data_all_oct <- filter(combined_data, pv_installation_year_month >= "2015-10-01" &  pv_installation_year_month < "2016-10-01")
combined_data_f <- vector_filter(combined_data, duration=60, 
                                 state="SA", standards=c("Transition"))
combined_data_all_oct <- vector_filter(combined_data_all_oct, duration=60, 
                                        state="SA", standards=c("AS4777.3:2005", "Transition","AS4777.2:2015"))
misfits <- filter(combined_data, pv_installation_year_month=="2020-01-28" & s_state=="SA" & d==60) 
combined_data_all_oct <- rbind(combined_data_all_oct, misfits)
agg_power <- vector_groupby(combined_data_f, agg_on_standard=TRUE)
agg_power_all_oct <- vector_groupby(combined_data_all_oct, agg_on_standard=TRUE)

plot(NULL, ylab="Power (kW)", xlab="Time", xlim=c(min(agg_power$Time), max(agg_power$Time)), ylim=c(0, 500))
lines(test_data$Time, test_data$Transition_period, lty=1, col="blue", lwd = 2)
lines(agg_power$Time, agg_power$Power_kW, lty=1, col="red", lwd = 2)
lines(agg_power_all_oct$Time, agg_power_all_oct$Power_kW, lty=2, col="green", lwd = 2)

# Add a legend
legend(x='bottomleft', y=NULL, legend=c("R method keep till 2016 Oct 09", "R method keep till 2016 Nov 01", "Naomi's aggregate"),
       col=c("red", "green", "blue"), lty=c(2, 1, 1), cex=0.8)
title("R method comparison for Transition")

dev.off()