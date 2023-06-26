#source("load_tool_environment.R")
#source('preprocess_cer_data/calc_installed_capacity_by_predictor_list.R')

main <- function(){
  region <- 'QLD'
  event_date <- '2021-05-25'
  event_lat <- -24.350597
  event_long <- 150.620694
  
  circ_sum_file <- "DERdat_results/20210525_SolAn_QLD_30s_circ_sum_220722.csv"
  circuit_summary <- read.csv(file = circ_sum_file, header = TRUE,
                              stringsAsFactors = FALSE)
  manufacturer_install_data <- read.csv(file = "inbuilt_data/monthly_cer_cumulative_capacity_all_processed.csv", 
                                        header = TRUE, stringsAsFactors = FALSE)
  manufacturer_install_data <- read.csv(file = "inbuilt_data/2022-11-15_cer_data_all_groupings.csv", 
                                        header = TRUE, stringsAsFactors = FALSE)
  # Predictor options: 'manufacturer', 'Standard_Version', 'Grouping', one of: 's_postcode', 'zone', 'distance'.
  predictors_list = c('manufacturer', 'Standard_Version', 'zone')
  
  #manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)
  #manufacturer_install_data <- calc_installed_capacity_by_all_predictors(manufacturer_install_data)
  #write.csv(manufacturer_install_data, "inbuilt_data/2022-11-15_monthly_cer_cumulative_capacity_all_processed.csv", row.names=FALSE)
  #manufacturer_capacitys <- get_manufacturer_capacitys(manufacturer_install_data, event_date, region)
  manufacturer_capacitys <- get_manufacturer_capacitys_variable(manufacturer_install_data, event_date, region, 
                                    replace(predictors_list, predictors_list %in% c('zone', 'distance'), 's_postcode'))
  if ('zone' %in% predictors_list | 'distance' %in% predictors_list){
    postcode_data <- read.csv(file=POSTCODE_DATA_FILE, header=TRUE, stringsAsFactors = FALSE)
    postcode_data <- process_postcode_data(postcode_data)
    manufacturer_capacitys <- get_distance_from_event(manufacturer_capacitys, postcode_data, event_lat, event_long)
    manufacturer_capacitys <- get_zones(manufacturer_capacitys, 100, 205, 1000)
    manufacturer_capacitys <- mutate(manufacturer_capacitys,  zone=ifelse(zone %in% c(NA), "NA", zone))
    
    circuit_summary <- subset(circuit_summary, select = -c(distance, lat, lon, zone))
    circuit_summary <- get_distance_from_event(circuit_summary, postcode_data, event_lat, event_long)
    circuit_summary <- get_zones(circuit_summary, 100, 205, 1000)
    circuit_summary <- mutate(circuit_summary,  zone=ifelse(zone %in% c(NA), "NA", zone))
  }
  
  manufacturer_capacitys <- manufacturer_capacitys %>% group_by_at(predictors_list) %>% 
    summarise(capacity=sum(capacity))
  
  circuits_to_summarise <- circuit_summary
  #upscale_existing_method(circuits_to_summarise, manufacturer_capacitys, min_sample=30, save_csvs = TRUE)
  upscale_new_glm_method(circuits_to_summarise, manufacturer_capacitys, predictors_list, min_sample=30, pre_event_CF,
                     file_to_save=sprintf('DERdat_results/20221014_event_analysis/20221014_SolAn_v2_model_m_st_z.csv', as.numeric(Sys.time())))
}

# existing method
upscale_existing_method <- function(circuits_to_summarise, manufacturer_capacitys, min_sample=30, file_to_save=NULL){
  disconnection_summary <- group_disconnections_by_manufacturer(circuits_to_summarise)
  disconnection_summary <- join_circuit_summary_and_cer_manufacturer_data(disconnection_summary, manufacturer_capacitys)
  disconnection_summary <- impose_sample_size_threshold(disconnection_summary, sample_threshold = min_sample)
  disconnection_summary <- calc_confidence_intervals_for_disconnections(disconnection_summary)
  disconnection_summary <- calc_upscale_kw_loss(disconnection_summary)
  upscaled_disconnections <- upscale_disconnections(disconnection_summary)
  
  kw_loss_estimate <- sum(upscaled_disconnections$predicted_kw_loss, na.rm=TRUE)
  if (!is.null(file_to_save)){
    write.csv(disconnection_summary, sprintf("%s_disconnection_summary_%s.csv", file_to_save, min_sample), 
              row.names = FALSE)
    write.csv(upscaled_disconnections, sprintf("%s_upscaled_disconnections_%s.csv", file_to_save, min_sample), 
              row.names = FALSE)
  }
  return(kw_loss_estimate)
}

# glm method
upscale_new_glm_method <- function(circuits_to_summarise, manufacturer_capacitys, predictors_list, pre_event_CF, 
                                   min_samples, file_to_save=NULL){
  circuits_data <- circuits_to_summarise %>% select(Standard_Version, Grouping, manufacturer, s_postcode, 
                                                    response_category, distance, zone)
  disconnection_categories <- c("3 Drop to Zero", "4 Disconnect")
  # TODO: check implications of not including UFLS as "bad"
  bad_categories <- c("6 Not enough data", "Undefined", "NA")
  circuits_data <- filter(circuits_data, !(response_category %in% bad_categories | is.na(response_category)))
  circuits_data <- circuits_data %>% mutate("disconnected" = ifelse(response_category %in% disconnection_categories,1,0))
  get_glm_estimate(circuits_data, manufacturer_capacitys, predictors_list, min_samples, pre_event_CF,
                   remove_others_from_model=FALSE, group_others_before_modelling=TRUE, file_to_save=file_to_save)
}

get_glm_estimate <- function(circuits_data, manufacturer_capacitys, predictors_list, pre_event_CF, min_samples=30,
                             remove_others_from_model=FALSE, group_others_before_modelling=TRUE, 
                             file_to_save=NULL){
  circuits_predictors_count <- circuits_data %>% group_by_at(predictors_list) %>% 
   summarise(disconnections = sum(disconnected), sample_size = length(response_category))
  circuits_data <- merge(circuits_data, circuits_predictors_count, by=predictors_list, all = TRUE)
  for (predictor in predictors_list){
    circuits_predictor_count <- circuits_data %>% group_by_at(predictor) %>% 
      summarise(disconnections = sum(disconnected),
                sample_size = length(response_category))
    circuits_data <- merge(circuits_data, circuits_predictor_count, by=predictor, suffixes=c("", paste("_",predictor,sep="")), all = TRUE)
  }

  # TODO: want each category to have a min sample of 30 not each bucket -> currently just checks 'manufacturer'
  
  if(remove_others_from_model) {
    circuits_data <- circuits_data %>% filter(!(manufacturer == "Unknown" | 
                                               manufacturer == "Multiple" |
                                               manufacturer == "Mixed" | 
                                               manufacturer == "" |
                                               is.na(manufacturer) |
                                               sample_size < min_samples))
  }
  if(group_others_before_modelling) {
    circuits_data <- mutate(circuits_data, manufacturer = ifelse(manufacturer == "Unknown" |
                                             manufacturer == "Multiple" |
                                             manufacturer == "Mixed" |
                                             manufacturer == "" |
                                             is.na(manufacturer),
                                             "Other", manufacturer))
    
    if('manufacturer' %in% predictors_list){
      circuits_data <- mutate(circuits_data, manufacturer = ifelse(sample_size_manufacturer < min_samples, 
                                                                   "Other", manufacturer))
    }
  }
  if('Standard_Version' %in% predictors_list){
    if((sum(circuits_data$Standard_Version=="Transition") < min_samples) | (sum(circuits_data$Standard_Version=="AS4777.3:2005") < min_samples)){
      circuits_data["Standard_Version"][circuits_data["Standard_Version"] == "Transition"] <- "AS4777.3:2005"
      manufacturer_capacitys["Standard_Version"][manufacturer_capacitys["Standard_Version"] == "Transition"] <- "AS4777.3:2005"
    }
    if(sum(circuits_data$Standard_Version=="AS4777.3:2005") < min_samples){
      circuits_data["Standard_Version"][circuits_data["Standard_Version"] == "AS4777.3:2005"] <- "AS4777.2:2015"
      manufacturer_capacitys["Standard_Version"][manufacturer_capacitys["Standard_Version"] == "AS4777.3:2005"] <- "AS4777.2:2015"
    }
    manufacturer_capacitys <- manufacturer_capacitys %>% group_by_at(predictors_list) %>% 
      summarise(capacity=sum(capacity))
  }
  
  model <- glm(reformulate(termlabels = predictors_list, response='disconnected'), circuits_data, 
               family=binomial, na.action=na.exclude)
  summary(model)
  
  # Create a dataframe with all possible combinations of the selected predictors
  unique_factors_list <- list()
  for(i in 1:length(predictors_list)) {
    unique_factors_list[[i]] <- unique(circuits_data[[predictors_list[i]]])
  }
  prediction_df <- expand.grid(unique_factors_list, stringsAsFactors = FALSE)
  colnames(prediction_df) <- predictors_list
  # alternative dataframe if we're only predicting well-defined buckets
  #prediction_df <- unique(circuits_data %>% select(all_of(predictors_list)))
  model_data <- data.frame(prediction_df, prediction=predict(model, prediction_df, type="response"), 
                           stringsAsFactors=FALSE)
  
  model_data <- merge(model_data, manufacturer_capacitys, by = predictors_list, all = TRUE)
  model_data <- merge(model_data, circuits_predictors_count, by = predictors_list, all = TRUE)
  
  if('manufacturer' %in% predictors_list){
    model_data <- mutate(model_data, manufacturer = ifelse(is.na(capacity), 'Other', manufacturer))
    model_data <- mutate(model_data, manufacturer = ifelse(is.na(prediction), 'Other', manufacturer))
  }
  
  if(!group_others_before_modelling){
    model_data <- mutate(model_data, manufacturer = ifelse(is.na(sample_size) | sample_size < min_samples,
                                                           'Other', manufacturer))
  
    # Create an Other group for manufacturers with a small sample size.
    model_data <- mutate(model_data, manufacturer = ifelse(manufacturer == "Unknown" | 
                                                           manufacturer == "Multiple" |
                                                           manufacturer == "Mixed" | 
                                                           manufacturer == "" |
                                                           is.na(manufacturer), 
                                                           "Other", manufacturer))
  }
  # Recalculate disconnection count and sample size.
  model_data <- group_by_at(model_data, predictors_list)
  model_data <- summarise(model_data, disconnections = sum(disconnections, na.rm = TRUE),
                          sample_size = sum(sample_size, na.rm = TRUE),
                          prediction = mean(prediction, na.rm = TRUE),
                          capacity = sum(capacity, na.rm = TRUE))
  
  # If tranche was not predicted, use the average disconnection rate for the whole dataset
  model_data <- mutate(model_data, prediction = ifelse(is.na(prediction), sum(disconnections) / sum(sample_size),
                                                       prediction))
  # TODO: develop a better method for unseen categories than setting to 0 - is this ever reached?
  model_data <- mutate(model_data, prediction = ifelse(is.na(prediction), 0, prediction))
  
  model_data$kwp_loss <- model_data$capacity * model_data$prediction
  # kw_loss_estimate <- model_data$kwp_loss
  kwp_loss_estimate <- sum(model_data$kwp_loss)
  if (!is.null(file_to_save)){
    write.csv(model_data, file_to_save, row.names = FALSE)
  }
  return(kwp_loss_estimate)
}

if (!interactive()) {
  main()
}
