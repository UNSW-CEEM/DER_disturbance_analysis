###PRIMARY 

####INPUT DATA####
#Start and End time of the data set
Start_time <- "2018-08-25 12:00:00"
End_time <- "2018-08-25 15:00:00"

#Input file locations
Fault_Datafile1 <- "Inputs/20180825/2018-05-25_sa_qld_fault_aemo.csv"
Fault_Datafile2 <- "Inputs/20180825/2018-05-25_act_tas_fault_aemo.csv"

Inverter_Datafile1 <- "Inputs/20180825/sites_details.csv"
Inverter_Datafile2 <- "Inputs/20180825/act_tas_sites_details.csv"

Circuit_Datafile1 <- "Inputs/20180825/circuit_details.csv"
Circuit_Datafile2 <- "Inputs/20180825/act_tas_circuit_details.csv"

CER_Datafile <-  "Inputs/CER_data.csv"

#FILTER FOR DATA TYPES (Duration and Site Types)
Duration_Read <- 60
Site_types <- c("pv_site_net", "pv_site")

#Event Times
t_0 <- "2018-08-25 13:11:55"
t_end_estimate_nadir <- "2018-08-25 13:13:55"
t_end <- "2018-08-25 13:30:55"


##Category Definitions
#AS POWER LOSS PERCENTAGES
#Category1 - Ride Through
Cat1_PL_perc <- 0.04
#Categories 2-6: Curtailment
Cat2_PL_perc <- 0.1 
Cat3_PL_perc <- 0.25
Cat4_PL_perc <- 0.5
Cat5_PL_perc <- 0.75
Cat6_PL_perc <- 0.1
#As a value in kW
#Category 7: Disconnect
Cat7_Disconnect_kW=0.1

Return_min_perc <- 0.1

####PROCESSES####
require("dplyr")
require("tidyr")
require("lubridate")
require("ggplot2")

#Read and Join Data Sets
#Outputs the file: adjusted_data_set
source("Read_Join_DataSets.R")

#Filter the data set as needed

filtered_data_set <- filter(adjusted_data_set, d == Duration_Read & con_type %in% Site_types)

write.csv(filtered_data_set, "Outputs/Joined_datasets.csv")

write.csv(CER.Capacity_updated, "Outputs/CER_buckets.csv")

##Clean data set
source("Clean_DataSet.R")

output_cleaned_dataset <- inner_join(filtered_data_set, output_clean_df, by=c("site_id", "c_id"))

write.csv(output_cleaned_dataset, "Outputs/cleaned_datasets.csv")

#Upscale dataset

source("Upscale_DataSet.R")


###TEST
test.a <- output_cleaned_dataset %>% 
  group_by(ts, s_state, Standard_Version) %>% 
  summarise(power=sum(power_kW))

ggplot(test.a, aes(ts, power, colour=Standard_Version, linetype=s_state))+
  geom_line()


write.csv(test.a, "Outputs/test.csv")
