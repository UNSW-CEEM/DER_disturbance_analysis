source("load_tool_environment.R")
source("upscale_disconnections/upscale_glm_test.R")
#install.packages('boot',dep=TRUE)
library(boot)

region <- 'QLD'
event_date <- '2021-05-25'
event_lat <- -24.350597
event_long <- 150.620694
circ_sum_file <- "DERdat_results/EQL-Luceo_report/20210525_Luceo_circ_sum_0504.csv"
#circ_sum_file <- "DERdat_results/20210525_Tesla_QLD_30s_circ_sum_300822.csv"
#circ_sum_file <- "DERdat_results/20210525_SolAn_QLD_30s_circ_sum_220722.csv"
#circ_sum_file <- "DERdat_results/20210525_alldata_QLD_30s_circ_sum.csv"
circuit_summary <- read.csv(file = circ_sum_file, header = TRUE, stringsAsFactors = FALSE)
#manufacturer_install_data <- read.csv(file = "inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv",
#header = TRUE, stringsAsFactors = FALSE)
#manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)
manufacturer_install_data <- read.csv(file = "inbuilt_data/cer_installs_by_postcode_manufacturer_size_monthly_processed.csv", 
                                      header = TRUE, stringsAsFactors = FALSE)

# Predictor options: 'manufacturer', 'Standard_Version', 'Grouping', one of: 's_postcode', 'zone', 'distance'.
predictors_list = c('manufacturer', 'Standard_Version')

manufacturer_capacitys <- get_manufacturer_capacitys_variable(manufacturer_install_data, event_date, region, 
                                                              replace(predictors_list,
                                                                      predictors_list %in% c('zone', 'distance'),
                                                                      's_postcode'))
if ('zone' %in% predictors_list | 'distance' %in% predictors_list){
  postcode_data <- read.csv(file=POSTCODE_DATA_FILE, header=TRUE, stringsAsFactors = FALSE)
  postcode_data <- process_postcode_data(postcode_data)
  manufacturer_capacitys <- get_distance_from_event(manufacturer_capacitys, postcode_data, event_lat, event_long)
  manufacturer_capacitys <- get_zones(manufacturer_capacitys, 200, 600, 1000)
  manufacturer_capacitys <- mutate(manufacturer_capacitys,  zone=ifelse(zone %in% c(NA), "NA", zone))
}

manufacturer_capacitys <- manufacturer_capacitys %>% group_by_at(predictors_list) %>% 
  summarise(capacity=sum(capacity))

circuits_to_summarise <- circuit_summary
circuits_data <- circuits_to_summarise %>% select(Standard_Version, Grouping, manufacturer, response_category, zone, sum_ac)
disconnection_categories <- c("3 Drop to Zero", "4 Disconnect")
bad_categories <- c("6 Not enough data", "Undefined", "NA")
circuits_data <- filter(circuits_data, !(response_category %in% bad_categories | is.na(response_category)))
circuits_data <- circuits_data %>% mutate("disconnected" = ifelse(response_category %in% disconnection_categories,1,0))

circuits_predictors_count <- circuits_data %>% group_by_at(predictors_list) %>% 
  summarise(disconnections = sum(disconnected), sample_size = length(response_category))
circuits_data <- merge(circuits_data, circuits_predictors_count, by=predictors_list, all = TRUE)
for (predictor in predictors_list){
  circuits_predictor_count <- circuits_data %>% group_by_at(predictor) %>% 
    summarise(disconnections = sum(disconnected),
              sample_size = length(response_category))
  circuits_data <- merge(circuits_data, circuits_predictor_count, by=predictor, suffixes=c("", paste("_",predictor,sep="")), all = TRUE)
}

circuits_data <- mutate(circuits_data, manufacturer = ifelse(manufacturer == "Unknown" |
                                                               manufacturer == "Multiple" |
                                                               manufacturer == "Mixed" |
                                                               manufacturer == "" |
                                                               is.na(manufacturer),
                                                             "Other", manufacturer))

if('manufacturer' %in% predictors_list){
  circuits_data <- mutate(circuits_data, manufacturer = ifelse(sample_size_manufacturer < 30, 
                                                               "Other", manufacturer))
}

#glm_formula <- reformulate(termlabels = predictors_list, response='disconnected')
glm_formula <- disconnected ~ manufacturer:Standard_Version
print(glm_formula)

cv_kfold_binary <- function(circuits_data, glm_formula, k_value=10){
  circuits_data_noNA <- circuits_data[!is.na(circuits_data$zone),]
  glm_model <- glm(glm_formula, circuits_data_noNA, family=binomial, na.action=na.exclude)
  cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
  print(cv.glm(circuits_data_noNA, glm_model, cost, K=k_value)$delta)
}

cv_whole_data <- function(circuits_data, glm_formula){
  cv_circuits_data <- circuits_data[!is.na(circuits_data$zone),]
  glm_model <- glm(glm_formula, circuits_data_noNA, family=binomial, na.action=na.exclude)
  cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
  cv_circuits_data$prediction <- predict(glm_model,cv_circuits_data, type="response")
  print(cost(cv_circuits_data$disconnected, cv_circuits_data$prediction))
  print(sum(cv_circuits_data$sum_ac * cv_circuits_data$prediction))
}

cv_upscaled_estimate <- function(circuits_data, glm_formula){
  circuits_data_shuffled <- circuits_data[!is.na(circuits_data$zone),]
  circuits_data_shuffled <- circuits_data_shuffled[sample(nrow(circuits_data_shuffled)),]
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(circuits_data_shuffled)),breaks=10,labels=FALSE)
  r_kw_losses <- c()
  p_kw_losses <- c()
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segment your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- circuits_data_shuffled[testIndexes, ]
    trainData <- circuits_data_shuffled[-testIndexes, ]
    #Use test and train data partitions however you desire...
    glm_model <- glm(glm_formula, trainData, family=binomial, na.action=na.exclude)
    testData$r_kw_loss <- testData$sum_ac * testData$disconnected
    testData$prediction <- predict(glm_model, testData, type="response")
    testData$p_kw_loss <- testData$sum_ac * testData$prediction
    r_kw_losses[i] <- sum(testData$r_kw_loss)
    p_kw_losses[i] <- sum(testData$p_kw_loss)
  }
  print(p_kw_losses)
  print(r_kw_losses)
}
  
cv_kfold_binary(circuits_data, glm_formula, 10)
cv_whole_data(circuits_data, glm_formula)
cv_upscaled_estimate(circuits_data, glm_formula)
