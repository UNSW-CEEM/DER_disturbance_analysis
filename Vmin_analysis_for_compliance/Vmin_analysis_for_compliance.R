# Ordering c_ids and then ts in database
data_compliance_5s_and_more <- data$combined_data_f[order(data$combined_data_f$c_id, data$combined_data_f$ts), ]

# Adding flag for low voltage condition 
data_compliance_5s_and_more <- cbind(data_compliance_5s_and_more, Flag_Vmin = ifelse(data_compliance_5s_and_more$vmin <= 231, TRUE, FALSE))

# adding flag for consecutive time steps being uniform to 5s intervals
data_compliance_5s_and_more<-data_compliance_5s_and_more%>%mutate(consecutive_ts = lead(ts) )
data_compliance_5s_and_more<-cbind(data_compliance_5s_and_more,Flag_t_consecutive=ifelse(data_compliance_5s_and_more$consecutive_ts-data_compliance_5s_and_more$ts==5,1,0))
data_compliance_5s_and_more <- cbind(data_compliance_5s_and_more, Flag_cumulative_ts = ifelse(data_compliance_5s_and_more$Flag_Vmin == TRUE & data_compliance_5s_and_more$Flag_t_consecutive == 1, 1, 0))

data_compliance_5s_and_more$Flag_cumulative_count<-cumsum(data_compliance_5s_and_more$Flag_cumulative_ts)
data_compliance_5s_and_more$Flag_cumulative_count<-data_compliance_5s_and_more$Flag_cumulative_count-cummax((data_compliance_5s_and_more$Flag_cumulative_ts==0)*data_compliance_5s_and_more$Flag_cumulative_count)

data_to_plot <- c(sum(data_compliance_5s_and_more$Flag_cumulative_count == 1,na.rm = TRUE), sum(data_compliance_5s_and_more$Flag_cumulative_count >= 2,na.rm = TRUE))
barplot(data_to_plot)

