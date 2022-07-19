source("load_tool_environment.R")
source("upscale_disconnections/upscale_glm_test.R")
#install.packages('boot',dep=TRUE)
library(boot)

region <- 'SA'
event_date <- '2022-03-05'
circuit_summary <- read.csv(file = "DERdat_results/20220305_event_analysis/20220305_circ_sum_1333.csv", header = TRUE, stringsAsFactors = FALSE)
manufacturer_install_data <- read.csv(file = "inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv", header = TRUE, stringsAsFactors = FALSE)
manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)
manufacturer_capacitys <- get_manufacturer_capacitys(manufacturer_install_data, event_date, region)
circuits_to_summarise <- circuit_summary
circuits_data <- circuits_to_summarise %>% select(Standard_Version, Grouping, manufacturer, response_category, zone)
disconnection_categories <- c("3 Drop to Zero", "4 Disconnect")
bad_categories <- c("6 Not enough data", "Undefined", "NA")
circuits_data <- filter(circuits_data, !(response_category %in% bad_categories | is.na(response_category)))
circuits_data <- circuits_data %>% mutate("disconnected" = ifelse(response_category %in% disconnection_categories,1,0))

predictors_list = c('manufacturer', 'Standard_Version')


function_1 <- function(data, i){
  d2 <- data[i,] 
  return(mean(d2$disconnected))
}

glm_wrapper <- function(data, i){
  d2 <- data[i,]
  get_glm_estimate(d2, manufacturer_capacitys, predictors_list)
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
