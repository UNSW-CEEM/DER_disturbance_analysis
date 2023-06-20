source("load_tool_environment.R")
source("upscale_disconnections/disc_upscaler.R")

# Inputs
region<-"SA"
event_date<-"2022-11-12"
event_lat<--35.285
event_long<-139.491

date_no_gaps <- str_replace_all(event_date, '-', '')
circ_sum_path <- sprintf("C:/Users/PDennis/Documents/DER_disturbance_analysis/DERdat_results/%s_%s.csv", date_no_gaps,
                         region)
circuit_summary <- read.csv(file = circ_sum_path, header = TRUE, stringsAsFactors = FALSE)

manufacturer_install_data <- read.csv(file = "inbuilt_data/monthly_cer_cumulative_capacity_all_processed.csv", 
                                      header = TRUE, stringsAsFactors = FALSE)

# Predictor options: 'manufacturer', 'Standard_Version', 'Grouping', one of: 's_postcode', 'zone', 'distance'.
predictors_list <- c('manufacturer', 'Standard_Version')

upscaler <- DisconnectionUpscaler$new(circuit_summary, manufacturer_install_data)
upscaler$get_installed_capacity_by_grouping(event_date, region)

if ('zone' %in% predictors_list | 'distance' %in% predictors_list){
  upscaler$add_distance_zones(POSTCODE_DATA_FILE, event_lat, event_long, c(100, 200, 400))
}

# Combine similar standards
if ('Standard_Version' %in% predictors_list){
  upscaler$combine_standards()
}

upscaler$make_plots()

upscaler$set_predictors(predictors_list)
upscaler$upscale_glm_method(30, 0.5)

upscaler$upscale_tranche_method(30, 0.5)

upscaler$run_bootstrap_CI(1000)
