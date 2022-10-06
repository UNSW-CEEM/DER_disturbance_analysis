source("load_tool_environment.R")
source("upscale_disconnections/upscale_glm_test.R")
#install.packages('boot',dep=TRUE)
library(boot)

region <- 'QLD'
event_date <- '2021-05-25'
event_lat <- -24.350597
event_long <- 150.620694
#circ_sum_file <- "DERdat_results/EQL-Luceo_report/20210525_Luceo_circ_sum_0504.csv"
#circ_sum_file <- "DERdat_results/20210525_Tesla_QLD_30s_circ_sum_300822.csv"
circ_sum_file <- "DERdat_results/20210525_SolAn_QLD_30s_circ_sum_220722.csv"
circuit_summary <- read.csv(file = circ_sum_file, header = TRUE, stringsAsFactors = FALSE)
#manufacturer_install_data <- read.csv(file = "inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv",
#header = TRUE, stringsAsFactors = FALSE)
#manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)
manufacturer_install_data <- read.csv(file = "inbuilt_data/cer_installs_by_postcode_manufacturer_size_monthly_processed.csv", 
                                      header = TRUE, stringsAsFactors = FALSE)

# Predictor options: 'manufacturer', Standard_Version', 'Grouping', one of: 's_postcode', 'zone', 'distance'.
predictors_list = c('manufacturer', 'Standard_Version', 'Grouping', 'zone')

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
circuits_data <- circuits_to_summarise %>% select(Standard_Version, Grouping, manufacturer, response_category, zone)
disconnection_categories <- c("3 Drop to Zero", "4 Disconnect")
bad_categories <- c("6 Not enough data", "Undefined", "NA")
circuits_data <- filter(circuits_data, !(response_category %in% bad_categories | is.na(response_category)))
circuits_data <- circuits_data %>% mutate("disconnected" = ifelse(response_category %in% disconnection_categories,1,0))

function_1 <- function(data, i){
  d2 <- data[i,] 
  return(mean(d2$disconnected))
}

glm_wrapper <- function(data, i){
  d2 <- data[i,]
  get_glm_estimate(d2, manufacturer_capacitys, predictors_list, file_to_save=NULL)
}

set.seed(1)
system.time(bootstrap_correlation <- boot(circuits_data, glm_wrapper, R=5000))

bootstrap_correlation

range(bootstrap_correlation$t)
mean(bootstrap_correlation$t)
sd(bootstrap_correlation$t)

boot.ci(boot.out=bootstrap_correlation, type=c("norm","basic","perc")) # ,"bca"))

boot_df <- data.frame(list(x=bootstrap_correlation$t, y=sequence(length(bootstrap_correlation$t))))

boot_model <- lm(x~., data=boot_df)
par(mfrow=c(2,2))
plot(boot_model)
hist(bootstrap_correlation$t, plot=TRUE, breaks=30)
