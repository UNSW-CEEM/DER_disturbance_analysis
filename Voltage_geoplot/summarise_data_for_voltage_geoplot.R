
# Voltage geo plot 
data_table_voltage_geoplot <- function(circuit_summary){
# circuit_summary<-read.csv("D:/Project_MATCH/DER_disturbance_analysis/DER_disturbance_analysis/validation/data/Jan_2021/test_circ_sum.csv")

  save_geograph_data<-matrix()  
  # colnames(save_geograph_data) <-c("Post Codes","lon","lat",Vmin","Vmax","Vmean")
  save_geograph_data <-circuit_summary
 #save_geograph_data<-filter(save_geograph_data,save_geograph_data$response_category == '4 Disconnect' | save_geograph_data$response_category == '3 Drop to Zero')
  save_geograph_data <- group_by(save_geograph_data, .dots=c("s_postcode"))
  save_geograph_data <-summarise(save_geograph_data ,  lat=first(lat), lon=first(lon),vmin_min=min(vmin_min))
  save_geograph_data <- as.data.frame(save_geograph_data)
#   
# print(save_geograph_data)
   
  save_geograph_data<-data.frame(save_geograph_data)
  return(save_geograph_data)

}
