source("load_tool_environment.R")

main <- function(){
  region <- 'SA'
  event_date <- '2022-03-05'
  circuit_summary <- read.csv(file = "DERdat_results/20220305_event_analysis/20220305_circ_sum_1333.csv", header = TRUE, stringsAsFactors = FALSE)
  manufacturer_install_data <- read.csv(file = "inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv", header = TRUE, stringsAsFactors = FALSE)
  
  
  manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)
  manufacturer_capacitys <- get_manufacturer_capacitys(manufacturer_install_data, event_date, region)
  #manufacturer_capacitys_no_standard <- manufacturer_capacitys %>% group_by(manufacturer) %>% summarise(capacity=sum(capacity))
  
  circuits_to_summarise <- circuit_summary
  #upscale_existing_method(circuits_to_summarise, manufacturer_capacitys)
  upscale_glm_method(circuits_to_summarise, manufacturer_capacitys)
}

# existing method
upscale_existing_method <- function(circuits_to_summarise, manufacturer_capacitys){
  disconnection_summary <- group_disconnections_by_manufacturer(circuits_to_summarise)
  disconnection_summary <- join_circuit_summary_and_cer_manufacturer_data(disconnection_summary, manufacturer_capacitys)
  disconnection_summary <- impose_sample_size_threshold(disconnection_summary, sample_threshold = 30)
  disconnection_summary <- calc_confidence_intervals_for_disconnections(disconnection_summary)
  disconnection_summary <- calc_upscale_kw_loss(disconnection_summary)
  upscaled_disconnections <- upscale_disconnections(disconnection_summary)
  
  write.csv(disconnection_summary, "disconnection_summary_30_v2.csv", row.names = FALSE)
  write.csv(upscaled_disconnections, "upscaled_disconnections_30_v2.csv", row.names = FALSE)
}

# glm method
upscale_glm_method <- function(circuits_to_summarise, manufacturer_capacitys){
  predictors_list = c('manufacturer', 'Standard_Version')
  circuits_data <- circuits_to_summarise %>% select(Standard_Version, Grouping, manufacturer, response_category, zone)
  disconnection_categories <- c("3 Drop to Zero", "4 Disconnect")
  bad_categories <- c("6 Not enough data", "Undefined", "NA")
  circuits_data <- filter(circuits_data, !(response_category %in% bad_categories | is.na(response_category)))
  circuits_data <- circuits_data %>% mutate("disconnected" = ifelse(response_category %in% disconnection_categories,1,0))
  get_glm_estimate(cicuits_data, manufacturer_capacitys, predictors_list, remove_others_from_model=FALSE, group_others_before_modelling=TRUE)
}

get_glm_estimate <- function(circuits_data, manufacturer_capacitys, predictors_list, remove_others_from_model=FALSE, group_others_before_modelling=TRUE){
  circuits_man_count <- circuits_data %>% group_by(manufacturer) %>% 
    summarise(disconnections = sum(disconnected), sample_size = length(response_category))
  circuits_predictors_count <- circuits_data %>% group_by_at(predictors_list) %>% 
    summarise(disconnections = sum(disconnected), sample_size = length(response_category))
  
  circuits_data <- merge(circuits_data, circuits_predictors_count, by=predictors_list, all = TRUE)
  
  if(remove_others_from_model) {
    circuits_data <- circuits_data %>% filter(!(manufacturer == "Unknown" | 
                                               manufacturer == "Multiple" |
                                               manufacturer == "Mixed" | 
                                               manufacturer == "" |
                                               is.na(manufacturer) |
                                               sample_size < 30))
  }
  if(group_others_before_modelling) {
    circuits_data <- mutate(circuits_data, manufacturer = ifelse(manufacturer == "Unknown" | 
                                             manufacturer == "Multiple" |
                                             manufacturer == "Mixed" | 
                                             manufacturer == "" |
                                             is.na(manufacturer)|
                                             sample_size < 30, 
                                             "Other", manufacturer))
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
  # prediction_df <- unique(circuits_data %>% select(all_of(predictors_list)))
  model_data <- data.frame(prediction_df, prediction=predict(model, prediction_df, type="response"), stringsAsFactors=FALSE)
  
  #model_data <- merge(model_data, manufacturer_capacitys_no_standard, by = 'manufacturer', all = TRUE)
  model_data <- merge(model_data, manufacturer_capacitys, by = c('Standard_Version', 'manufacturer'), all = TRUE)
  model_data <- merge(model_data, circuits_predictors_count, by = predictors_list, all = TRUE)
  
  model_data <- mutate(model_data, manufacturer = ifelse(is.na(capacity), 'Other', manufacturer))
  model_data <- mutate(model_data, manufacturer = ifelse(is.na(prediction), 'Other', manufacturer))
  
  if(!group_others_before_modelling){
    model_data <- mutate(model_data, manufacturer = ifelse(is.na(sample_size) | sample_size < 30, 'Other', manufacturer))
  
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
  
  model_data$kw_loss <- model_data$capacity * model_data$prediction
  kw_loss_estimate <- sum(model_data$kw_loss)
  #write.csv(model_data, sprintf('DERdat_results/model_disc_boot_%s.csv', as.numeric(Sys.time())), row.names = FALSE)
  return(kw_loss_estimate)
}
