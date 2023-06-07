source("load_tool_environment.R")
source("upscale_disconnections/upscale_glm_test.R")
source("upscale_disconnections/bootstrap_test.R")

# Inputs
region<-"SA"
event_date<-"2022-11-12"
event_lat<--35.285
event_long<-139.491


date_no_gaps <- str_replace_all(event_date, '-', '')
circ_sum<-sprintf("%s_%s.csv", date_no_gaps, region)
circ_sum<-"20221112_circ_sum_35min_window_5m_response.csv"
#underlying_summary<-sprintf("%s_%s.csv", "QLD_underlying", date_no_gaps)
#circuit_summary <- distinct(circuit_summary, c_id, clean, .keep_all = TRUE) %>% filter(clean=="cleaned")
#write.csv(circuit_summary, "C:/Users/SunSPoT/OneDrive - UNSW/05_Upscaling/PSSE validation circuit summaries/20181009/20181009_QLD.csv", row.names=FALSE)

# Predictor options: 'manufacturer', 'Standard_Version', 'Grouping', one of: 's_postcode', 'zone', 'distance'.
predictors_list <- c('manufacturer', 'Standard_Version', 'Grouping')
cer_data <- "new"


circ_sum_path <- sprintf("C:/Users/SunSPoT/OneDrive - UNSW/05_Upscaling/PSSE validation circuit summaries/%s/%s", date_no_gaps, circ_sum)
circ_sum_path <- sprintf("E:/Project_MATCH/DER_disturbance_analysis/DERdat_results/20221112_event_analysis/%s", circ_sum)
circuit_summary <- read.csv(file = circ_sum_path, header = TRUE, stringsAsFactors = FALSE)

if(cer_data=="original"){
  manufacturer_install_data <- read.csv(file = "inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv", 
                                        header = TRUE, stringsAsFactors = FALSE)
  manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)
  manufacturer_capacitys <- get_manufacturer_capacitys(manufacturer_install_data, event_date, region)
}
if(cer_data=="new"){
  manufacturer_install_data <- read.csv(file = "inbuilt_data/monthly_cer_cumulative_capacity_all_processed.csv", 
                                      header = TRUE, stringsAsFactors = FALSE)
  manufacturer_capacitys <- get_manufacturer_capacitys_variable(manufacturer_install_data, event_date, region, 
  replace(predictors_list, predictors_list %in% c('zone', 'distance'), 's_postcode'))
}

if ('zone' %in% predictors_list | 'distance' %in% predictors_list){
  postcode_data <- read.csv(file=POSTCODE_DATA_FILE, header=TRUE, stringsAsFactors = FALSE)
  postcode_data <- process_postcode_data(postcode_data)
  manufacturer_capacitys <- get_distance_from_event(manufacturer_capacitys, postcode_data, event_lat, event_long)
  manufacturer_capacitys <- get_zones(manufacturer_capacitys, 100, 200, 400)
  manufacturer_capacitys <- mutate(manufacturer_capacitys,  zone=ifelse(zone %in% c(NA), "NA", zone))
  
  circuit_summary <- subset(circuit_summary, select = -c(distance, lat, lon, zone))
  circuit_summary <- get_distance_from_event(circuit_summary, postcode_data, event_lat, event_long)
  circuit_summary <- get_zones(circuit_summary, 100, 200, 400)
  circuit_summary <- mutate(circuit_summary,  zone=ifelse(zone %in% c(NA), "NA", zone))
}

manufacturer_capacitys <- manufacturer_capacitys %>% group_by_at(predictors_list) %>% 
  summarise(capacity=sum(capacity))

circuits_data <- circuit_summary %>% select(Standard_Version, Grouping, manufacturer, s_postcode, 
                                            response_category, distance, zone)

# Combine similar standards
circuits_data["Standard_Version"][circuits_data["Standard_Version"] == "Transition"] <- "AS4777.3:2005"
manufacturer_capacitys["Standard_Version"][manufacturer_capacitys["Standard_Version"] == "Transition"] <- "AS4777.3:2005"
circuits_data["Standard_Version"][circuits_data["Standard_Version"] == "Transition 2020-21"] <- "AS4777.2:2015"
manufacturer_capacitys["Standard_Version"][manufacturer_capacitys["Standard_Version"] == "Transition 2020-21"] <- "AS4777.2:2015"
circuits_data["Standard_Version"][circuits_data["Standard_Version"] == "AS4777.2:2015 VDRT"] <- "AS4777.2:2015"
manufacturer_capacitys["Standard_Version"][manufacturer_capacitys["Standard_Version"] == "AS4777.2:2015 VDRT"] <- "AS4777.2:2015"

disconnection_categories <- c("3 Drop to Zero", "4 Disconnect")
bad_categories <- c("6 Not enough data", "Undefined", "NA", "UFLS Dropout")
circuits_data <- filter(circuits_data, !(response_category %in% bad_categories | is.na(response_category)))
circuits_data <- circuits_data %>% mutate("disconnected" = ifelse(response_category %in% disconnection_categories,1,0))

make_plots <- function(circuits_data, manufacturer_capacitys, predictors_list){
  for (predictor in predictors_list){
    predictor_capacitys <- manufacturer_capacitys %>% group_by_at(predictor) %>% 
      summarise(capacity=sum(capacity))
    circuits_predictors_count <- circuits_data %>% group_by_at(predictor) %>% 
      summarise(disconnections = sum(disconnected), sample_size = length(response_category))
    circuits_data_counts <- merge(circuits_data, circuits_predictors_count, by=predictor, all = TRUE)
    if (predictor == "manufacturer"){
      circuits_data_counts <- mutate(circuits_data_counts, manufacturer = ifelse(manufacturer == "Unknown" |
                                                                   manufacturer == "Multiple" |
                                                                   manufacturer == "Mixed" |
                                                                   manufacturer == "" |
                                                                   is.na(manufacturer),
                                                                 "Other", manufacturer))
    }
    predictor_data <- group_by_at(circuits_data_counts, predictor)
    predictor_data <- summarise(predictor_data, disconnections = first(disconnections, na.rm = TRUE),
                                sample_size = first(sample_size, na.rm = TRUE),
                                disc_rate = disconnections/sample_size)
    
    predictor_data <- merge(predictor_data, predictor_capacitys, by = predictor, all = TRUE)
    if (predictor == "manufacturer"){
      predictor_data <- mutate(predictor_data, manufacturer = ifelse(is.na(sample_size), 'Other', manufacturer))
    }
    predictor_data <- group_by_at(predictor_data, predictor)
    predictor_data <- summarise(predictor_data, disconnections = sum(disconnections, na.rm = TRUE),
                                sample_size = sum(sample_size, na.rm = TRUE),
                                disc_rate = disconnections/sample_size,
                                capacity = sum(capacity, na.rm = TRUE))
    predictor_data$sample_prop = predictor_data$sample_size / sum(predictor_data$sample_size, na.rm = TRUE)
    predictor_data$capacity_prop = predictor_data$capacity / sum(predictor_data$capacity, na.rm = TRUE)
    
    # Create predictor evaluation plots
    par(mar=c(12,4,4,2))
    # 1. Disconnection rates between categories
    counts <- select(predictor_data, disc_rate)
    barp <- barplot(t(as.matrix(counts)), names=predictor_data[[predictor]],
            main="Proportion of each category observed to disconnect", col=rainbow(nrow(counts)), ylim=c(0,1),
            xlab="", ylab="Proportion of sites that disconnected", beside=TRUE, las=2)
    text(barp, t(as.matrix(counts)) + 0.1, labels = round(t(as.matrix(counts)),digits=2))
        grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
    # 2. Sample and CER proportions in each category
    counts <- select(predictor_data, c(sample_prop, capacity_prop))
    barp <- barplot(t(as.matrix(counts)), names=predictor_data[[predictor]], legend=c("Sample data", "CER data"),
            main="Proportion of each category within sample and CER", col=rainbow(2), ylim=c(0,1),
            xlab="", ylab="Proportion of sites", beside=TRUE, las=2)
    text(barp, t(as.matrix(counts)) + 0.05, labels = round(t(as.matrix(counts)),digits=2))
    grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
    # 3. Number of samples in each category
    counts <- select(predictor_data, sample_size)
    barp <- barplot(t(as.matrix(counts)), names=predictor_data[[predictor]],
            main="Number of samples in each category", col=rainbow(nrow(counts)),
            xlab="", ylab="Number of sites", beside=TRUE, las=2)
    text(barp, t(as.matrix(counts)) + 10, labels = t(as.matrix(counts)))
    lines(x=t(as.matrix(counts)), y=rep(30, nrow(counts)))
    grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
    
    #write.csv(predictor_data, sprintf('DERdat_results/PSSE_event_validation/20170303_SA_%s.csv', predictor), row.names = FALSE)
  }
}

#make_plots(circuits_data, manufacturer_capacitys, predictors_list)

#upscale_existing_method(circuit_summary, manufacturer_capacitys, min_sample=30, file_to_save='DERdat_results/PSSE_event_validation/20181009_QLD')
#upscale_existing_method(circuit_summary, manufacturer_capacitys, min_sample=30, file_to_save='DERdat_results/20221112_event_analysis/20221112_glm_st_sg_z.csv')
#get_glm_estimate(circuits_data, manufacturer_capacitys, predictors_list, min_sample=30, 
#                   file_to_save=sprintf('DERdat_results/PSSE_event_validation/20170303_SA_glm_st_v2.csv', as.numeric(Sys.time())))
get_glm_estimate(circuits_data, manufacturer_capacitys, predictors_list, min_sample=30, 
                   file_to_save="DERdat_results/20221112_event_analysis/20221112_glm_m_st_sg_combstd.csv")

print(get_bootstrap_confidence_interval(circuits_data, manufacturer_capacitys, predictors_list, num_repetitions = 1000))
