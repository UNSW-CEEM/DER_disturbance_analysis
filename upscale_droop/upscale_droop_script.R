########################################### PARAMATERS and External Functions #####################################

# Make sure to highlight and run the two functions at the bottom of the script first before running! 

source("load_tool_environment.R")
source("preprocess_cer_data/calc_installed_capacity_by_standard_and_manufacturer.R")
source("process_input_data/process_input_data_functions.R")
source("upscale_disconnections/summarise_disconnections.R")
source("confidence_intervals/clopper_pearson_binomial_confidence_interval.R")
source("upscale_droop/upscale_droop_functions.R")

event_date <- "2022-11-12"
region <- "SA"
site_norm <- FALSE # If true, no external capacity factor used
external_capacity_factor <- 0.28


underlying_data_file <- "C:/Users/mtrollip/Local/GitHub/DER_disturbance_analysis/data/2022-11-12/phoebe_results_longer_window/20221112_underlying_35min_window.csv"
CER_install_data_file <- "C:/Users/mtrollip/Local/GitHub/DER_disturbance_analysis/inbuilt_data/cer_cumulative_capacity_and_number.csv"
CER_install_manufacturer_data_file <- "C:/Users/mtrollip/Local/GitHub/DER_disturbance_analysis/inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv"

# Where you want to store your outputted results
output_directory <- "C:/Users/mtrollip/Local/GitHub/DER_disturbance_analysis/data/2022-11-12/phoebe_results_longer_window/droop_scale_response"

########################################### Read in Data ###################################

# Underlying data 
UD <- read.csv(file=underlying_data_file, header = TRUE, stringsAsFactors = FALSE)
UD_raw <- UD

########################################### Process Data ###################################

# Here we:
#1. filter out bad sites
#2. filter out standards that arent expected to provide droop
#3. overwrite the 2015 VDRT and Transition 2020-21 standards as 2015 Standard 
#4. Sub in 2020 droop rsponse column into 2015 droop response column for 2020 circuits (the 2015 droop response
#   column becomes the combined droop response column)
#5. bucket Compliant and Non-compliant responding together as one group 
#6. Bucket together droop response and standard into a combined standard and compliance column

# Filter out sites that arent expected to perform droop response

standards_with_droop <- c('AS4777.2:2015', 'AS4777.2:2015 VDRT', 'AS4777.2:2020', 'Transition 2020-21')

UD <- filter(UD_raw, Standard_Version %in% standards_with_droop)

# # Filter out any sites with "bad data" 
BadCategories <- c("Not enough data", "Undefined", "UFLS Dropout")
# Select distinct site_id/response_category combos
BadSiteIds <- group_by(UD_raw, site_id, compliance_status) %>% summarise()
# Filter to get a list of site_ids to remove
# Note that R reads in the NAs as actual NA values which is annoying. You do you R.
BadSiteIds <- filter(BadSiteIds,compliance_status %in% BadCategories | is.na(compliance_status))
# Remove bad site_ids. Note the ! negates the %in% operator to make in 'not in'
UD <- filter(UD,!site_id %in% BadSiteIds$site_id)

# Group 2015 VDRT and Transition 2020-21 into 2015 Standard
UD <- mutate(UD, Standard_Version = 
                            ifelse(Standard_Version %in% c("AS4777.2:2015 VDRT", "Transition 2020-21"),
                                   "AS4777.2:2015", Standard_Version))

# write in 2020 standard into the droop compliance column if standard is 2020
UD <- mutate(UD, compliance_status = ifelse(Standard_Version == "AS4777.2:2020", compliance_status_2020, compliance_status))

# Concatenate standards with droop response (for grouping)
UD <- mutate(UD, compliance_status = ifelse(compliance_status %in% c("Non-compliant Responding", "Compliant"), "Responding",
                                            ifelse(compliance_status == "Non-compliant", "Not Responding",  compliance_status)))

UD <- mutate(UD, StdComplianceCombined = paste(Standard_Version, compliance_status))

######################## Get proportion (by count) of each droop response per standard and OEM  ###############################

Proportions <- group_by(UD, Standard_Version, c_id, compliance_status, manufacturer) %>% summarise()
Proportions <- mutate(Proportions, StdComplianceCombined = paste(Standard_Version, compliance_status))
TotalPerStandard_and_OEM <- group_by(Proportions, Standard_Version, manufacturer) %>% summarise(Sample_size=n())
Proportions <- group_by(Proportions, Standard_Version, StdComplianceCombined, manufacturer) %>% summarise(Count=n())
Proportions <- left_join(Proportions, TotalPerStandard_and_OEM, by = c("Standard_Version","manufacturer"))
Proportions <- mutate(Proportions, Proportion = Count/Sample_size)
  

################################### Get CER installed capacity per group ##########################################

#1. gets capacity per Standard and OEM
#2. Assigns portions of that capacity to each group based on Proportions in previous section 

# Get fleet capacity data
cer_install_data <- read.csv(CER_install_data_file,
                             header = TRUE, stringsAsFactors = FALSE)
manufacturer_install_data <- read.csv(CER_install_manufacturer_data_file,
                                      header = TRUE, stringsAsFactors = FALSE)

# Get manufacturer installed capacity for each OEM and all Standards with droop compliance at the time of the event
manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)
manufacturer_install_data <- get_manufacturer_capacitys(manufacturer_install_data, event_date, region)
manufacturer_install_data <- filter(manufacturer_install_data, Standard_Version %in% standards_with_droop)
# combined VDRT and Transition with 2015
manufacturer_install_data <- mutate(manufacturer_install_data, Standard_Version = 
                                      ifelse(Standard_Version %in% c("AS4777.2:2015 VDRT", "Transition 2020-21"),
                                             "AS4777.2:2015", Standard_Version))
manufacturer_install_data <- group_by(manufacturer_install_data, Standard_Version, s_state, manufacturer)
manufacturer_install_data <- summarise(manufacturer_install_data, capacity = sum(capacity))

Proportions <-  merge(Proportions, manufacturer_install_data, by = c('Standard_Version', 'manufacturer'), 
                      all = TRUE)

# Combined any OEM with less than 30 samples into 'Other' group. Correctly adjust 'Other' install capacity
Proportions <- impose_sample_size_threshold_compliance(Proportions, 30)

# Get capacity per droop response as a proportion of total isntalled capacity for that OEM and Standard. 
Proportions <- mutate(Proportions, proportion_capacity = proportion*capacity)
  
  
##################################### Upscale MW profile by OEM ###################################
# Two options here
# Option 1: Use a 'site performance factor'. The power profile of a site is divided by the sites max capacity. 
#           The average site performance factor for given OEM, Standard and Droop response is used to represent the capacity factor per timestep for that class. 
#           The average site performance factor is multiplied by the proportional installed capacity of that OEM, Standard and droop response (stored in Proportions)
#           The result is summed over all OEMs for that given Standard and droop response to provide the upscaled MW profile per droop response class and Standard 

# Option 2: Use 'external capacity factor'. The power profile is normalised to its output immediately before the event 
#           (i.e this gives an output of 1 just before event). 
#           The normalised power trace is multiplied by the proportional installed capacity for that OEM, Standard and droop response (store in Proportions)
#           This would mean each class would at be at its maximum capacity output immediately before the event (as normalised value is 1 at pre event interval)
#           the traces are then scaled by the external capacity factor, 0.X, such that the outputs is at X% its maximum capacity during the pre event interval   

###################################################################################################################

# Option 1
if(site_norm) {
  # Get the average site performance factor in underlyng data for each class per timestep
   site_performance_factor <- group_by(UD, site_id, ts) %>%
     summarise(site_performance_factor = first(site_performance_factor),
               manufacturer = first(manufacturer), Standard_Version = first(Standard_Version),
               StdComplianceCombined = first(StdComplianceCombined))
  
   # frst join UD with proportions to identify which OEMs have been assigned to "other" (come up as NA in proportion_cap)
   site_performance_factor <- left_join(site_performance_factor, Proportions[c("StdComplianceCombined", "proportion_capacity", "manufacturer")],
                                        by = c("StdComplianceCombined","manufacturer"))
   
   # combine any OEM's into 'Other' that are < 30 samples or have unknown, multiple / mixed. This apears as with a proportion capacity of 'na' following the join  
   site_performance_factor <- combine_erroneous_OEMs(site_performance_factor)
   
   
  # Combine and get average site performance capacity factor for each class with the OEMs > 30 samples. 
   site_performance_factor <- group_by(site_performance_factor, ts,
                                       manufacturer, Standard_Version, StdComplianceCombined) %>%
      summarise(average_site_performance_factor = mean(site_performance_factor))
  
   
   # add back the proportion capacities
   upscale_MW_profile_OEM <- left_join(site_performance_factor, Proportions[c("StdComplianceCombined", "proportion_capacity", "manufacturer")],
                                       by = c("StdComplianceCombined","manufacturer"))
   
   # upscale per class 
   upscale_MW_profile_OEM <- mutate(upscale_MW_profile_OEM, upscale_MW = average_site_performance_factor*proportion_capacity/1000)
   
   # Combine the total from each OEM
   upscale_MW_profile <- group_by(upscale_MW_profile_OEM, ts, StdComplianceCombined)
   upscale_MW_profile <- summarise(upscale_MW_profile, upscale_MW = sum(upscale_MW))

   pivot_upscaled_MW_profile <- pivot_wider(select(upscale_MW_profile, c("ts","StdComplianceCombined", "upscale_MW")), names_from = StdComplianceCombined,
                                             values_from =upscale_MW)
   write.csv(pivot_upscaled_MW_profile, paste(output_directory,"droop_compliance_upscale_by_OEM_site_normalisation.csv",sep=""), row.names = FALSE)

  
} else{
  c_id_norm_power <- UD[c('ts', 'c_id', 'c_id_norm_power', 'manufacturer', 'Standard_Version', 'StdComplianceCombined')]

    
    # Add OEMs < 30 samples or that dont exist in CER into 'other'.  
    c_id_norm_power <- left_join(c_id_norm_power, Proportions[c("StdComplianceCombined", "proportion_capacity", "manufacturer")], by = c("StdComplianceCombined","manufacturer"))
    
    # For all OEMs in the c_id_norm_power df that are not present in the list of OEMs with n>30, set these 
    # manufacturers to 'other'
    c_id_norm_power <- combine_erroneous_OEMs(c_id_norm_power)
    
    #Recombine and get an average normalised power profile for each class
    average_c_id_norm_power <- group_by(c_id_norm_power, ts, manufacturer, StdComplianceCombined, Standard_Version) %>%
      summarise(average_c_id_norm_power = mean(c_id_norm_power))
    
    # add back the porportion capacities
    upscale_MW_profile_OEM <- left_join(average_c_id_norm_power, 
                                         Proportions[c("StdComplianceCombined", "proportion_capacity", "manufacturer")],
                                         by = c("StdComplianceCombined","manufacturer"))
    
    
    # upscale per class 
    upscale_MW_profile_OEM <- mutate(upscale_MW_profile_OEM, upscale_MW = average_c_id_norm_power*proportion_capacity/1000)
    
    
    # Combine the total per timestamp from each OEM for each Standard and response type
    upscale_MW_profile <- group_by(upscale_MW_profile_OEM, ts, StdComplianceCombined)
    upscale_MW_profile <- summarise(upscale_MW_profile, upscale_MW = sum(upscale_MW))
  

    # filter out Off at t0 class as not applicable when dividing by pre-event interval output (will be dividing by 0)
    upscale_MW_profile <- filter(upscale_MW_profile, !StdComplianceCombined %in% c("AS4777.2:2015 Off at t0", "AS4777.2:2020 Off at t0"))
    
    # Get fleet capacity per Standard and droop response type
    Prop_per_class <- group_by(Proportions, StdComplianceCombined) %>% summarise(total_capacity = sum(proportion_capacity)/1000)
    
    # Add fleet capacity per Standard and droop response type to profile and divide to re-normalise the traces
    upscale_MW_profile <- left_join(upscale_MW_profile, Prop_per_class, by = c("StdComplianceCombined"))
    upscale_MW_profile <- mutate(upscale_MW_profile, normalised_power = upscale_MW/total_capacity)
    upscale_MW_profile <- mutate(upscale_MW_profile, upscale_MW = upscale_MW * external_capacity_factor)
    
    
    pivot_upscaled_MW_profile <- pivot_wider(select(upscale_MW_profile, c("ts","StdComplianceCombined", "upscale_MW")), names_from = StdComplianceCombined,
                                            values_from =upscale_MW)
    
    # Get the average c_id norm power per class based on 
    write.csv(pivot_upscaled_MW_profile, paste(output_directory,"droop_compliance_upscale_by_OEM_external_cap_factor_norm.csv", sep=""), row.names = FALSE)
    
    
}




