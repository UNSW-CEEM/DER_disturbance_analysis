source("upscale_disconnections/upscale_glm_test.R")
source("upscale_disconnections/bootstrap_test.R")

DisconnectionUpscaler <- R6Class("DisconnectionUpscaler",
  public = list(
    predictor_list = NULL,
    allowed_predictors = c('manufacturer', 'Standard_Version', 'Grouping', 's_postcode', 'zone', 'distance'),
    circuits_data = NULL,
    circuits_to_summarise = NULL,
    cer_install_data = NULL,
    weighting_capacities = NULL,
    event_date = NULL,
    region = NULL,
    zones_added = FALSE,
    pred_in_cer_data = NULL,
    initialize = function(circuits_to_summarise = NA, cer_install_data = NA, event_date = NA, region = NA) {
      self$circuits_to_summarise <- circuits_to_summarise
      self$circuits_data <- self$get_circuit_data(circuits_to_summarise)
      self$cer_install_data <- cer_install_data
      self$event_date <- event_date
      self$region <- region
      self$pred_in_cer_data <- intersect(self$allowed_predictors, colnames(cer_install_data))
      self$get_installed_capacity_by_grouping()
    },
    set_predictors = function(predictor_list) {
      if ('zone' %in% predictor_list & !self$zones_added) {
        stop("Distance zones must be added using 'upscaler$add_distance_zones' before predictor 'zone' can be used")
      }
      if (all(predictor_list %in% self$pred_in_cer_data)) {
        self$predictor_list <- predictor_list
      } else if (all(predictor_list %in% self$allowed_predictors) & !all(predictor_list %in% self$pred_in_cer_data)) {
        stop(paste("At least one of the predictors entered is not in the provided CER data. Select from:", 
                   paste(upscaler$pred_in_cer_data, collapse=', ')))
      } else {
        stop(paste("At least one of the predictors entered is not valid. Select from:",
                   paste(self$allowed_predictors, collapse=', ')))
      }
      #TODO - also need to validate that predictors work with dataset e.g. enough samples
    },
    assess_predictors = function(predictor) {
      # Perform an assessment to determine which predictors to recommend
      predictor_data <- self$get_predictor_summary(predictor)
      disc_diff <- max(predictor_data$disc_rate) - min(predictor_data$disc_rate)
      max_prop_diff <- max(abs(predictor_data$sample_prop - predictor_data$capacity_prop))
      predictor_data$samples_gt_30 <- predictor_data$sample_size > 30
      num_categories <- dim(predictor_data)[1]
      categories_with_30_samples <- sum(predictor_data$samples_gt_30)
      #TODO: what to return? Set of numerical outputs, binaries or string summary
    },
    get_installed_capacity_by_grouping = function(){
      # Replaces "get_manufacturer_capacitys()"
      cer_install_data <- self$cer_install_data
      cer_install_data <- filter(cer_install_data, s_state == self$region)
      cer_install_data <- cer_install_data[order(cer_install_data$date), ]
      cer_install_data <- filter(cer_install_data, date <= self$event_date)
      cer_install_data <- group_by_at(cer_install_data, append(self$pred_in_cer_data, 's_state'))
      cer_install_data <- summarise(cer_install_data, capacity = last(standard_capacity))
      cer_install_data <- as.data.frame(cer_install_data)
      self$weighting_capacities <- cer_install_data
    },
    add_distance_zones = function(POSTCODE_DATA_FILE, event_lat, event_long, zone_radii) {
      # Calculate the distance zones based on postcode data for both CER data and the circuit_summary
      postcode_data <- read.csv(file=POSTCODE_DATA_FILE, header=TRUE, stringsAsFactors = FALSE)
      postcode_data <- process_postcode_data(postcode_data)
      weighting_capacities <- get_distance_from_event(self$weighting_capacities, postcode_data, event_lat, event_long)
      weighting_capacities <- get_zones(weighting_capacities, zone_radii[1], zone_radii[2], zone_radii[3])
      weighting_capacities <- mutate(weighting_capacities,  zone=ifelse(zone %in% c(NA), "NA", zone))
      self$weighting_capacities <- weighting_capacities
      
      circuit_summary <- self$circuits_to_summarise
      circuit_summary <- subset(circuit_summary, select = -c(distance, lat, lon, zone))
      circuit_summary <- get_distance_from_event(circuit_summary, postcode_data, event_lat, event_long)
      circuit_summary <- get_zones(circuit_summary, zone_radii[1], zone_radii[2], zone_radii[3])
      circuit_summary <- mutate(circuit_summary,  zone=ifelse(zone %in% c(NA), "NA", zone))
      self$circuits_to_summarise <- circuit_summary
      self$zones_added <- TRUE
      self$pred_in_cer_data <- intersect(self$allowed_predictors, colnames(weighting_capacities))
    },
    combine_standards = function() {
      # Group the transition standards in with the previous standard
      self$circuits_data["Standard_Version"][self$circuits_data["Standard_Version"] == "Transition"] <-"AS4777.3:2005"
      self$weighting_capacities["Standard_Version"][self$weighting_capacities["Standard_Version"] == "Transition"] <- "AS4777.3:2005"
      self$circuits_data["Standard_Version"][self$circuits_data["Standard_Version"] == "Transition 2020-21"] <- "AS4777.2:2015"
      self$weighting_capacities["Standard_Version"][self$weighting_capacities["Standard_Version"] == "Transition 2020-21"] <- "AS4777.2:2015"
      self$circuits_data["Standard_Version"][self$circuits_data["Standard_Version"] == "AS4777.2:2015 VDRT"] <- "AS4777.2:2015"
      self$weighting_capacities["Standard_Version"][self$weighting_capacities["Standard_Version"] == "AS4777.2:2015 VDRT"] <- "AS4777.2:2015"
    },
    get_circuit_data = function(circuits_to_summarise) {
      # Could be private
      circuits_data <- circuits_to_summarise %>% select(Standard_Version, Grouping, manufacturer, s_postcode, 
                                                        response_category, distance, zone)
      disconnection_categories <- c("3 Drop to Zero", "4 Disconnect")
      # Don't count circuits without a well defined response type
      # In future we may want the option to include UFLS Dropout
      bad_categories <- c("6 Not enough data", "Undefined", "NA", "UFLS Dropout")
      circuits_data <- filter(circuits_data, !(response_category %in% bad_categories | is.na(response_category)))
      circuits_data <- circuits_data %>% 
        mutate("disconnected" = ifelse(response_category %in% disconnection_categories, 1, 0))
      return(circuits_data)
    },
    upscale_glm_method = function(pre_event_CF, min_sample=30) {
      # Run GLM upscaling method
      if (is.null(self$predictor_list)) {
        stop("Predictors must be set before running this upscaling method")
      }
      weighting_capacities <- self$weighting_capacities %>% group_by_at(self$predictor_list) %>%
        summarise(capacity=sum(capacity))
      losses <- list()
      losses$kwp_loss_estimate <- get_glm_estimate(self$circuits_data, weighting_capacities, self$predictor_list,
                                                   pre_event_CF, min_sample, remove_others_from_model=FALSE,
                                                   group_others_before_modelling=TRUE, file_to_save=NULL)
      losses$kw_loss_estimate <- losses$kwp_loss_estimate * pre_event_CF / 100
      losses$perc_loss_estimate <- 0 # TODO: implement this
      # TODO: decide on best return object
      return(losses)
    },
    upscale_tranche_method = function(pre_event_CF, min_sample=30) {
      # Run tranche upscaling method
      weighting_capacities <- self$weighting_capacities %>% group_by_at(c('Standard_Version', 'manufacturer')) %>%
        summarise(capacity=sum(capacity))
      disconnection_summary <- group_disconnections_by_manufacturer(self$circuits_to_summarise)
      disconnection_summary <- join_circuit_summary_and_cer_manufacturer_data(disconnection_summary,
                                                                              weighting_capacities)
      disconnection_summary <- impose_sample_size_threshold(disconnection_summary, sample_threshold = min_sample)
      disconnection_summary <- calc_confidence_intervals_for_disconnections(disconnection_summary)
      disconnection_summary <- calc_upscale_kw_loss(disconnection_summary)
      upscaled_disconnections <- upscale_disconnections(disconnection_summary)
      kwp_loss_estimate <- sum(upscaled_disconnections$predicted_kw_loss, na.rm=TRUE)
      # TODO: decide on best return object
      return(kwp_loss_estimate)
    },
    run_bootstrap_CI = function(min_sample=30, num_repetitions=5000) {
      # Run bootstrap method
      # Return confidence interval
      weighting_capacities <- self$weighting_capacities %>% group_by_at(self$predictor_list) %>%
        summarise(capacity=sum(capacity))
      boot_ci_results <- get_bootstrap_confidence_interval(self$circuits_data, weighting_capacities,
                                                           self$predictor_list, min_sample,
                                                           num_repetitions = num_repetitions)
      return(boot_ci_results)
    },
    get_predictor_summary = function(predictor) {
      # Create table summarising the disconnection rate of each category and comparing count + percentage to CER data
      if (predictor=='zone' & !self$zones_added) {
        stop("Distance zones must be added using 'upscaler$add_distance_zones' before predictor 'zone' can be used")
      }
      manufacturer_capacitys <- self$weighting_capacities
      circuits_data <- self$circuits_data
      
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
      return(predictor_data)
    },
    make_plots = function(predictor) {
      # Create plots used to evaluate the predictors
      # TODO - replace plots with interactive plotly charts?
      predictor_data <- self$get_predictor_summary(predictor)
      
      # Create predictor evaluation plots
      plots <- list()
      par(mar=c(12,4,4,2))
      # 1. Disconnection rates between categories
      counts <- select(predictor_data, disc_rate)
      barp <- barplot(t(as.matrix(counts)), names=predictor_data[[predictor]],
                      main="Proportion of each category observed to disconnect", col=rainbow(nrow(counts)),
                      ylim=c(0,1), xlab="", ylab="Proportion of sites that disconnected", beside=TRUE, las=2)
      text(barp, t(as.matrix(counts)) + 0.1, labels = round(t(as.matrix(counts)),digits=2))
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
      plots$disc_plot = recordPlot()
      dev.off()
      # 2. Sample and CER proportions in each category
      counts <- select(predictor_data, c(sample_prop, capacity_prop))
      barp <- barplot(t(as.matrix(counts)), names=predictor_data[[predictor]], legend=c("Sample data", "CER data"),
                      main="Proportion of each category within sample and CER", col=rainbow(2), ylim=c(0,1),
                      xlab="", ylab="Proportion of sites", beside=TRUE, las=2)
      text(barp, t(as.matrix(counts)) + 0.05, labels = round(t(as.matrix(counts)),digits=2))
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
      plots$bias_plot = recordPlot()
      dev.off()
      # 3. Number of samples in each category
      counts <- select(predictor_data, sample_size)
      barp <- barplot(t(as.matrix(counts)), names=predictor_data[[predictor]],
                      main="Number of samples in each category", col=rainbow(nrow(counts)),
                      xlab="", ylab="Number of sites", beside=TRUE, las=2)
      text(barp, t(as.matrix(counts)) + 10, labels = t(as.matrix(counts)))
      lines(x=t(as.matrix(counts)), y=rep(30, nrow(counts)))
      grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
      plots$sample_count_plot = recordPlot()
      dev.off()
      
      #write.csv(predictor_data, sprintf('DERdat_results/PSSE_event_validation/20170303_SA_%s.csv', predictor),
      #  row.names = FALSE)
      
      return (plots)
    }
  )
)
