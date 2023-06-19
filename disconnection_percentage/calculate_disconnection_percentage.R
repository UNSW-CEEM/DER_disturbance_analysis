
# Code for finding percentage disconnected/Dropped to Zero DPVs at specific voltage ranges 

# function to find the overall percentage of disconnected/dropped to zero at each zone
calc_percentage_disconnect_or_droptozero_DPVs <- function(circuit_summary){
  
  # obtaining data from circuit summary needed for this analysis (response category, zone, vmin, vmax)
  r <- data.frame(response_category = circuit_summary$response_category, zone = circuit_summary$zone,
                  v_min = circuit_summary$vmin_min,v_max=circuit_summary$vmax_max )
  
  # count disconnected or drop to zero DPVs at each zone
  find_rows_disconnected_or_droptozero<-filter(r,r$response_category == '4 Disconnect' | r$response_category == '3 Drop to Zero')
  total_DPVs_disconnected_or_droptozero<-nrow(find_rows_disconnected_or_droptozero)
  
  x_graph<-c("1 Zone","2 Zone","3 Zone")
  legend_graph<-c("Only Vmin<180V","180V<Vmin<200V","200V<Vmin<220V", "220V<Vmin<240V","240V<Vmin<260V","260V<Vmax<265V","Only Vmax>265V","Both Vmin<180V & Vmax>265V")
 
  save_graph_data_count<-matrix(nrow=length(x_graph),ncol=length(legend_graph)) 
  save_graph_data<-matrix(nrow=length(x_graph),ncol=length(legend_graph))
  colnames(save_graph_data) <- legend_graph
  rownames(save_graph_data)<-x_graph
  
  total_DPVs_each_zone<-matrix(length(x_graph))
  
  for(k in 1:length(x_graph)){
    for(i in 1:length(legend_graph)){
     
      total_DPVs_each_zone[k]<-as.numeric(nrow(filter(r,r$zone == x_graph[k])))     # total DPVs in each zone
      
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
  
  save_graph_data<-data.frame(save_graph_data)
  return(save_graph_data)
}
