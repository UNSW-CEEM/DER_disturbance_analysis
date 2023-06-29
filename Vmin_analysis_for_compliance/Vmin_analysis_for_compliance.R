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

# data_to_plot <- c(sum((data_compliance_5s_and_more$Flag_cumulative_count == 1&data_compliance_5s_and_more$zone==1),na.rm = TRUE), sum((data_compliance_5s_and_more$Flag_cumulative_count&&data_compliance_5s_and_more$zone==1) >= 2,na.rm = TRUE))
# barplot(data_to_plot)

x_graph<-c("1 Zone","2 Zone","3 Zone")
data_to_plot<-matrix(0,3,2)

for(k in 1:length(x_graph)){
data_to_plot[k,1] <- c(sum((data_compliance_5s_and_more$Flag_cumulative_count == 1&data_compliance_5s_and_more$zone == x_graph[k]),na.rm = TRUE))
data_to_plot[k,2]<-c(sum(data_compliance_5s_and_more$Flag_cumulative_count == 2&data_compliance_5s_and_more$zone == x_graph[k],na.rm = TRUE))
}

barplot(data_to_plot)


##### uniqur c_id count
id1<-c(0,1,0,0,1,2,0,0,0,0,1,2,3,4,0,0,0,0,0,1,0)
id2<-c(1,2,0,0,1,2,0,0,0,0,0,1,2,0,0,0,0,0,0,0,0)
id3<-c(1,2,0,0,1,0,0,0,1,0,0,1,2,3,0,0,1,0,1,0,0)
id4<-c(0,0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,0,0,0,0)
id<-c(id1,id2,id3,id4)
  
mat1<-c(rep("id1", times = length(id1)),rep("id2", times = length(id1)),rep("id3", times = length(id3)),rep("id4", times = length(id4)))
mat2<-matrix()

mat2[,1]<-mat1
mat2[,2]<-id





