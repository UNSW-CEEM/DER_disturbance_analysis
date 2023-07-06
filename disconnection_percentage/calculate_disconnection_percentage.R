
### Finding percentage disconnected/Dropped to Zero DPVs at specific voltage ranges ###

# function to find the overall percentage of disconnected/dropped to zero at each zone
calc_percentage_disconnect_or_droptozero_DPVs <- function(circuit_summary,combined_data_filtered){
  
  # obtaining data from circuit summary needed for this analysis (response category, zone, vmin, vmax)
  r <- data.frame(response_category = circuit_summary$response_category, zone = circuit_summary$zone,
                  v_min = circuit_summary$vmin_min,v_max=circuit_summary$vmax_max )
  
  # count disconnected or drop to zero DPVs at each zone
  find_rows_disconnected_or_droptozero<-filter(r,r$response_category == '4 Disconnect' | r$response_category == '3 Drop to Zero')
  
  x_graph<-c("1 Zone","2 Zone","3 Zone")
  legend_graph<-c("Only Vmin<180V","180V<Vmin<200V","200V<Vmin<220V", "220V<Vmin<240V","240V<Vmin<260V","260V<Vmax<265V","Only Vmax>265V","Both Vmin<180V & Vmax>265V", "total DPVs as per cct summary","total DPVs disconnected/drop to zero as per cct summary","only Vmin<180V for 5s","Only Vmin<180V for 10s","Only Vmin<180V for more than 10s","total disconnection as per time series data")
 
  save_graph_data_count<-matrix(nrow=length(x_graph),ncol=length(legend_graph)) 
  save_graph_data<-matrix(nrow=length(x_graph),ncol=length(legend_graph))
  colnames(save_graph_data) <- legend_graph
  rownames(save_graph_data)<-x_graph
  
  total_DPVs_each_zone<-matrix(length(x_graph))
  total_DPVs_disconnected_each_zone<-matrix(length(x_graph))
  
  for(k in 1:length(x_graph)){
    for(i in 1:length(legend_graph)){
     
      total_DPVs_each_zone[k]<-as.numeric(nrow(filter(r,r$zone == x_graph[k])))     # total DPVs in each zone
      total_DPVs_disconnected_each_zone[k]<- as.numeric(nrow(filter(find_rows_disconnected_or_droptozero,find_rows_disconnected_or_droptozero$zone == x_graph[k]))) # total DPVs in each zone disconnected/drop to zero
      
      # counting disconnected/drop to zero DPVs in specific voltage ranges  
      save_graph_data_count[k,1]<-nrow(filter(find_rows_disconnected_or_droptozero,find_rows_disconnected_or_droptozero$zone == x_graph[k] & find_rows_disconnected_or_droptozero$v_min < 180 & !(find_rows_disconnected_or_droptozero$v_max >=265)))
      save_graph_data_count[k,2]<-nrow(filter(find_rows_disconnected_or_droptozero,find_rows_disconnected_or_droptozero$zone == x_graph[k] & find_rows_disconnected_or_droptozero$v_max <200 & find_rows_disconnected_or_droptozero$v_max >=180))                              
      save_graph_data_count[k,3]<-nrow(filter(find_rows_disconnected_or_droptozero,find_rows_disconnected_or_droptozero$zone == x_graph[k] & find_rows_disconnected_or_droptozero$v_max <220 & find_rows_disconnected_or_droptozero$v_max >=200))
      save_graph_data_count[k,4]<-nrow(filter(find_rows_disconnected_or_droptozero,find_rows_disconnected_or_droptozero$zone == x_graph[k] & find_rows_disconnected_or_droptozero$v_max <240 & find_rows_disconnected_or_droptozero$v_max >=220))
      save_graph_data_count[k,5]<-nrow(filter(find_rows_disconnected_or_droptozero,find_rows_disconnected_or_droptozero$zone == x_graph[k] & find_rows_disconnected_or_droptozero$v_max <260 & find_rows_disconnected_or_droptozero$v_max >=240))
      save_graph_data_count[k,6]<-nrow(filter(find_rows_disconnected_or_droptozero,find_rows_disconnected_or_droptozero$zone == x_graph[k] & find_rows_disconnected_or_droptozero$v_max <265 & find_rows_disconnected_or_droptozero$v_max >=260))
      save_graph_data_count[k,7]<-nrow(filter(find_rows_disconnected_or_droptozero,find_rows_disconnected_or_droptozero$zone == x_graph[k] & find_rows_disconnected_or_droptozero$v_max >265 & !(find_rows_disconnected_or_droptozero$v_min < 180) )) 
      save_graph_data_count[k,8]<-nrow(filter(find_rows_disconnected_or_droptozero,find_rows_disconnected_or_droptozero$zone == x_graph[k] & find_rows_disconnected_or_droptozero$v_max >265 & find_rows_disconnected_or_droptozero$v_min < 180 )) 
      
      # percentage calculations                               
      save_graph_data[k,i]<-as.numeric(save_graph_data_count[k,i])/total_DPVs_each_zone[k]*100
    }
  }
  

  
# analysing disconnection percentage when voltage<180V separately for compliance
  
data_compliance_5s_and_more <- combined_data_filtered[order(combined_data_filtered$c_id, combined_data_filtered$ts), ]   # Arrranging c_ids and then ts in database
data_compliance_5s_and_more<-filter(data_compliance_5s_and_more,data_compliance_5s_and_more$response_category == '4 Disconnect' | data_compliance_5s_and_more$response_category == '3 Drop to Zero') # filtering 'disconnect' or 'drop to zero'
data_compliance_5s_and_more$vmin[is.na(data_compliance_5s_and_more$vmin)] <- 230 # Replacing NA values of Vmin to 230V

# Adding flag for low voltage condition 
data_compliance_5s_and_more <- cbind(data_compliance_5s_and_more, Flag_Vmin = ifelse(data_compliance_5s_and_more$vmin <= 180, TRUE, FALSE))

# adding flag for consecutive time steps being uniform to 5s intervals
data_compliance_5s_and_more<-data_compliance_5s_and_more%>%mutate(consecutive_ts = lead(ts) )
data_compliance_5s_and_more<-cbind(data_compliance_5s_and_more,Flag_t_consecutive=ifelse(data_compliance_5s_and_more$consecutive_ts-data_compliance_5s_and_more$ts==5,1,0))
data_compliance_5s_and_more <- cbind(data_compliance_5s_and_more, Flag_cumulative_ts = ifelse(data_compliance_5s_and_more$Flag_Vmin == TRUE & data_compliance_5s_and_more$Flag_t_consecutive == 1, 1, 0))

data_compliance_5s_and_more$Flag_cumulative_count<-cumsum(data_compliance_5s_and_more$Flag_cumulative_ts)
data_compliance_5s_and_more$Flag_cumulative_count<-data_compliance_5s_and_more$Flag_cumulative_count-cummax((data_compliance_5s_and_more$Flag_cumulative_ts==0)*data_compliance_5s_and_more$Flag_cumulative_count)

# Check Vmin continues for the next 5s, 10s or "more than 10s" 
timecheck <- function(cumulative_count) {
  present_count <- data_compliance_5s_and_more$Flag_cumulative_count[cumulative_count]
  consecutive_counts_5s <- data_compliance_5s_and_more$Flag_cumulative_count[(cumulative_count+1)]
  consecutive_counts_10s <- data_compliance_5s_and_more$Flag_cumulative_count[(cumulative_count+1):(cumulative_count+2)]
  consecutive_counts_greater_than_10s <- data_compliance_5s_and_more$Flag_cumulative_count[(cumulative_count+1):(cumulative_count+2)]
  
  flag_5s<-all(c(present_count, consecutive_counts_5s) == c(1,0))
  flag_10s<-all(c(present_count, consecutive_counts_10s) == c(1,2,0))
  flag_more_than_10s<-all(c(present_count, consecutive_counts_greater_than_10s) == c(1,2,3))  
  
  return(list(flag_5s,flag_10s,flag_more_than_10s))
} 


check_Vmin_timestep<-matrix(0,nrow=length(data_compliance_5s_and_more$Flag_cumulative_count),ncol=1)


for (i in 1:(nrow(check_Vmin_timestep)-2)){
  
  if (timecheck(i)[[2]]=="TRUE"){
    check_Vmin_timestep[i]<- "10s"
  } else if(timecheck(i)[[1]]=="TRUE"){
    check_Vmin_timestep[i]<- "5s"
  } else if(timecheck(i)[[3]]=="TRUE"){
    check_Vmin_timestep[i]<- "more than 10s"
  }
}

data_compliance_5s_and_more<-cbind(data_compliance_5s_and_more, check_Vmin_timestep)

# counting c_id for disconnections in 5sec
grouped_5s <- data_compliance_5s_and_more %>%
  group_by(zone,c_id)%>%
  summarize(only_5s=any(check_Vmin_timestep=="5s")&!any(check_Vmin_timestep=="10s")&!any(check_Vmin_timestep=="more than 10s"))%>%
  filter(only_5s)  %>%
  summarise(unique_id_5s=n())

# counting c_id for disconnections in 10sec
grouped_10s <- data_compliance_5s_and_more %>%
  group_by(zone,c_id) %>%
  summarize(only_10s=any(check_Vmin_timestep=="10s")&!any(check_Vmin_timestep=="5s")&!any(check_Vmin_timestep=="more than 10s"))%>%
  filter(only_10s)  %>%
  summarise(unique_id_10s=n())

# counting c_id for disconnections in "more than 10sec"
grouped_more_than_10s <- data_compliance_5s_and_more %>%
  group_by(zone,c_id) %>%
  summarize(more_than_10s=any(check_Vmin_timestep=="more than 10s"))%>%
  filter(more_than_10s)  %>%
  summarise(unique_id_more_than_10s=n())

grouped_total_disconnection <- data_compliance_5s_and_more %>%
  group_by(zone,c_id) %>%
  summarize(response_cat=any(response_category=="4 Disconnect")| any(data_compliance_5s_and_more$response_category == "3 Drop to Zero"))%>%
  filter(response_cat)  %>%
  summarise(unique_id_total_disconnects=n())

   save_graph_data[,11]<-grouped_5s$unique_id_5s[1:3]/total_DPVs_each_zone*100
   save_graph_data[,12]<-grouped_10s$unique_id_10s[1:3]/total_DPVs_each_zone*100
   save_graph_data[,13]<-grouped_more_than_10s$unique_id_more_than_10s[1:3]/total_DPVs_each_zone*100
   save_graph_data[,14]<-grouped_total_disconnection$unique_id_total_disconnects[1:3]
  
    # Replacing NA values of save_graph_data
   save_graph_data[is.na(save_graph_data)] <- 0
   
   # data for total DPVs and total DPVs disconnected/drop to zero (count)
   save_graph_data[,9]<-total_DPVs_each_zone # as per circuit summary total DPVs
   save_graph_data[,10]<-total_DPVs_disconnected_each_zone # as per circuit summary total DPVs disconnected/drop to zero

save_graph_data<-data.frame(save_graph_data)
return(save_graph_data)
}