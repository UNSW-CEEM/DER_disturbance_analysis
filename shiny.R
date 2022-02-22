source("load_tool_environment.R")

# set up logging at debug level
basicConfig(level = 10)
addHandler(writeToFile, logger="shinyapp", file="logging/applogs.log")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  # Allows for the use of notifications.
  useShinyjs(),
  titlePanel("PV System Disturbance Analysis"),
  # Input Bar
  tabsetPanel(
    tabPanel("Main", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(id = "side_panel",
          h4("Settings input file selection"),
          textInput("settings_file", "Select JSON file for loading inputs (optional).",
                    value = "C:/Users/NGorman/Documents/GitHub/DER_disturbance_analysis/test.json"
          ),
          shinyFilesButton("load_settings", "Choose File", "Select settings file ...", multiple = FALSE),
          tags$hr(),
          h4("File selection"),
          textInput("database_name", "SQLite database file",
                    value = "C:/Users/NGorman/Documents/GitHub/DER_disturbance_analysis/data/20201203/20201203.db"
          ),
          fluidRow(
            div(style="display:inline-block", shinyFilesButton("choose_database", "Choose File", 
                                                               "Select database file ...", multiple = FALSE)),
            div(style="display:inline-block", actionButton("load_file_from_settings", "Load from settings file")),
            div(style="display:inline-block", actionButton("connect_to_database", "Connect"))),
          tags$hr(),
          uiOutput("load_date"),
          uiOutput("load_time_start"),
          uiOutput("load_time_end"),
          radioButtons("region_to_load", label = strong("Regions"), 
                       choices = list("QLD","NSW", "VIC", "SA", "TAS", "WA"), selected = "TAS", inline = TRUE),
          uiOutput("duration"),
          HTML("<br><br>"),
          textInput("frequency_data", "Frequency data file", 
                    value = ""
          ),
          shinyFilesButton("choose_frequency_data", "Choose File", "Select fequency data file ...", multiple = FALSE),
          HTML("<br><br>"),
          actionButton("load_first_filter_settings", "Load from settings file"),
          actionButton("load_data", "Load data"),
          tags$hr(),
          h4("Category Filter"),
          uiOutput("cleaned"),
          uiOutput("StdVersion"),
          uiOutput("size_groupings"),
          uiOutput("responses"),
          uiOutput("zones"),
          uiOutput("compliance"),
          uiOutput("reconnection_compliance"),
          uiOutput("postcodes"),
          uiOutput("manufacturers"),
          uiOutput("models"),
          uiOutput("sites"),
          uiOutput("circuits"),
          uiOutput("offsets"),
          tags$hr(),
          h4("Chart specific filters"),
          materialSwitch("norm_power_filter_off_at_t0", 
                         label = strong("Normalised power chart: filter out off at t0 circuits:"), 
                         status = "primary", value = TRUE),
          tags$hr(),
          h4("Grouping Categories"),
          materialSwitch("standard_agg", label=strong("AS4777:"), status="primary", value=TRUE),
          materialSwitch("grouping_agg", label=strong("Size Grouping:"), status="primary", value=TRUE),
          materialSwitch("response_agg", label=strong("Response Grouping:"), status="primary", value=FALSE),
          materialSwitch("pst_agg", label=strong("Postcodes:"), status="primary", value=FALSE),
          materialSwitch("manufacturer_agg", label=strong("Manufacturer:"),status="primary", value=FALSE),
          materialSwitch("model_agg", label=strong("Models:"), status="primary", value=FALSE),
          materialSwitch("circuit_agg", label=strong("Circuits:"), status="primary", value=FALSE),
          materialSwitch("zone_agg", label=strong("Zones:"), status="primary", value=FALSE),
          materialSwitch("compliance_agg", label=strong("Compliance:"), status="primary", value=FALSE),
          materialSwitch("reconnection_compliance_agg", label=strong("Reconnection Compliance:"), status="primary", value=FALSE),
          tags$hr(),
          h4("Additional Processing"),
          radioButtons("confidence_category", label = strong("Grouping category to calculate confidence interval for, 
                                                              must be a Grouping Category"), 
                       choices = list("none", "response_category", "compliance_status", "reconnection_compliance_status"), selected = "none", inline = TRUE),
          materialSwitch(inputId="raw_upscale", label=strong("Upscaled Data"), status="primary", right=FALSE),
          tags$hr(),
          h4("Event information"),
          uiOutput("event_date"),
          uiOutput("pre_event_interval"),
          uiOutput("window_length"),
          uiOutput("post_event_ufls_window_length"),
          uiOutput("event_latitude"),
          uiOutput("event_longitude"),
          uiOutput("zone_one_radius"),
          uiOutput("zone_two_radius"),
          uiOutput("zone_three_radius"),
          tags$hr(),
          uiOutput("update_plots"),
          actionButton("load_second_filter_settings", "Load from settings file"),
          shinySaveButton("save_settings", "Save settings file", "Save file as ...", filetype=list('json'))
        ),
        #Output
        mainPanel(
          plotlyOutput(outputId="PlotlyTest"),
          uiOutput("save_agg_power"),
          HTML("<br>"),
          uiOutput("save_underlying"),
          HTML("<br>"),
          uiOutput("save_circuit_summary"),
          HTML("<br>"),
          uiOutput("batch_save"),
          HTML("<br>"),
          uiOutput("save_ideal_response"),
          HTML("<br>"),
          uiOutput("save_ideal_response_downsampled"),
          HTML("<br>"),
          uiOutput("save_manufacturer_disconnection_summary"),
          HTML("<br>"),
          uiOutput("save_manufacturer_disconnection_summary_with_separate_ufls_counts"),
          HTML("<br>"),
          uiOutput("save_upscaled_disconnection_summary"),
          HTML("<br>"),
          uiOutput("save_upscaled_disconnection_summary_with_separate_ufls_counts"),
          HTML("<br><br>"),
          plotlyOutput(outputId="NormPower"),
          plotlyOutput(outputId="Frequency"),
          plotlyOutput(outputId="Voltage"),
          plotlyOutput(outputId="ResponseCount"),
          uiOutput("save_response_count"),
          plotlyOutput(outputId="distance_response"),
          uiOutput(outputId="save_distance_response"),
          plotlyOutput(outputId="ZoneCount"),
          uiOutput("save_zone_count"),
          plotlyOutput(outputId="map"),
          HTML("<br><br>"),
          dataTableOutput("sample_count_table"),
          HTML("<br><br>"),
          uiOutput("save_sample_count")

        )
      )
    ),
    tabPanel("Data Cleaning", fluid=TRUE, 
      mainPanel(
        plotlyOutput("site_plot"),
        h4("Editing the tables below changes the connected database, to use these changes in the analysis data must
           be reloaded on the main tab."),
        h4("Cleaned site data (select to view trace)"),
        DTOutput('site_details_editor'),
        h4("Cleaned Circuit data (select to view trace)"),
        DTOutput('circuit_details_editor')
      )
    ),
    tabPanel("Manual compliance", fluid=TRUE, 
     mainPanel(
       plotlyOutput("compliance_plot"),
       h4("Editing the compliance value changes the connected database, to use these changes in the analysis data must
           be reloaded on the main tab."),
       uiOutput("manual_compliance_type"),
       uiOutput("compliance_cleaned_or_raw"),
       uiOutput("compliance_circuits"),
       uiOutput("set_c_id_compliance"),
       fluidRow(
         div(style="display:inline-block", uiOutput("get_previous_c_id")),
         div(style="display:inline-block", uiOutput("get_next_c_id")))
     )
    ),
    tabPanel("Settings", fluid=TRUE, 
             sidebarLayout(
               sidebarPanel(id="side_panel",
                            h3("Droop response compliance settings"),
                            numericInput("compliance_threshold", 
                                         label = strong('Compliance threshold'), 
                                         value = 0.5, max=1, min=0),
                            numericInput("start_buffer", 
                                         label = strong('Start buffer, allowed time to reach compliance threshold, in seconds.'), 
                                         value = 60),
                            numericInput("end_buffer", 
                                         label = strong('End buffer, allowed time for system ending response early, in seconds.'), 
                                         value = 60),
                            numericInput("end_buffer_responding", 
                                         label = strong('Response time, window length for systems to be considered Non Compliant Responding, in seconds.'), 
                                         value = 120),
                            h3("Reconnection compliance settings"),
                            numericInput("reconnection_threshold", 
                                         label = strong('The level at which a circuit is considered to have reconnected.'), 
                                         value = 0.95, max = 1, min = 0),
                            numericInput("ramp_rate_threshold", 
                                         label = strong('Reconnection ramp rate threshold for assessing compliance, in pct/min.'), value = 0.333),
                            numericInput("total_ramp_threshold_for_compliance", 
                                         label = strong('Total ramp threshold for compliance, in pct'), value = 0.125),
                            numericInput("total_ramp_threshold_for_non_compliance", 
                                         label = strong('Toatl ramp threshold for non compliance, in pct'), value = 0.25),
                            numericInput("ramp_rate_change_resource_limit_threshold", 
                                         label = strong('Ramp rate change threshold for detecting resource limitation, in pct/min'), value = -0.1),
                            h3("UFLS settings"),
                            numericInput("pre_event_ufls_window_length", 
                                         label = strong('Pre-event UFLS Window: The time window before the 
                                                         event used to determine if the connection with a 
                                                         device is stable enough to determine its UFLS status, 
                                                         in minutes.'), 
                                         value = 5),
                            numericInput("pre_event_ufls_stability_threshold", 
                                         label = strong('The fraction of the Pre-event UFLS Window that needs to be 
                                                         sampled for the connection with a device to be considered
                                                         stable enough to determine its UFLS status.'), 
                                         value = 0.6, max = 1, min = 0),
                            h3("Misc settings"),
                            numericInput("disconnecting_threshold", 
                                         label = strong('Disconnecting threshold, level below which circuit is considered to
                                                      have disconnected. Note that this value is used in the compliance
                                                        calculations but NOT the response categorisation.'),
                                         value = 0.05, max = 1, min = 0),
                            numericInput("NED_threshold", 
                                         label = strong('Minimum proportion of sampled seconds allowed within post event interval to not have a 6 Not enough data response'), 
                                         value = 0.8, max = 1, min = 0),
                            materialSwitch("exclude_solar_edge", label = strong("Exclude solar edge from reconnection summary."), 
                                           status = "primary", value = FALSE),
                            materialSwitch("exclude_islanded_circuits", label = strong("Exclude islanded circuits from figures and results"),
                                           status = "primary", value = TRUE),
                            actionButton("load_backend_settings", "Load from settings file")
                            ),
               mainPanel()
               )
             ),
    tabPanel("Assumptions and Methodology", fluid=TRUE, documentation_panel())
  ),
  useShinyalert()
)

reset_sidebar <- function(input, output, session, stringsAsFactors) {
  output$cleaned <- renderUI({})
  output$postcodes <- renderUI({})
  output$manufacturers <- renderUI({})
  output$models <- renderUI({})
  output$sites <- renderUI({})
  output$circuits <- renderUI({})
  output$size_groupings <- renderUI({})
  output$StdVersion <- renderUI({})
  output$responses <- renderUI({})
  output$zones <- renderUI({})
  output$compliance <- renderUI({}) 
  output$reconnection_compliance <- renderUI({}) 
  output$offsets <- renderUI({})
  shinyjs::hide("norm_power_filter_off_at_t0")
  shinyjs::hide("standard_agg")
  shinyjs::hide("raw_upscale")
  shinyjs::hide("pst_agg")
  shinyjs::hide("grouping_agg")
  shinyjs::hide("grouping_agg")
  shinyjs::hide("manufacturer_agg")
  shinyjs::hide("response_agg")
  shinyjs::hide("circuit_agg")
  shinyjs::hide("zone_agg")
  shinyjs::hide("compliance_agg")
  shinyjs::hide("reconnection_compliance_agg")
  shinyjs::hide("save_settings")
  shinyjs::hide("load_second_filter_settings")
  shinyjs::hide("confidence_category")
  output$event_date <- renderUI({})
  output$pre_event_interval <- renderUI({})
  output$window_length <- renderUI({})
  output$post_event_ufls_window_length <- renderUI({})
  output$event_latitude <- renderUI({})
  output$event_longitude <- renderUI({})
  output$zone_one_radius <- renderUI({})
  output$zone_two_radius <- renderUI({})
  output$zone_three_radius <- renderUI({})
  output$update_plots <- renderUI({})
}

reset_chart_area <- function(input, output, session, stringsAsFactors) {
  output$PlotlyTest <- renderPlotly({})
  output$save_agg_power <- renderUI({})
  output$save_underlying <- renderUI({})
  output$save_circuit_summary <- renderUI({})
  output$sample_count_table <- renderDataTable({})
  output$save_sample_count <- renderUI({})
  output$NormPower <- renderPlotly({})
  output$ResponseCount <- renderPlotly({})
  output$save_response_count <- renderUI({})
  output$ZoneCount <- renderPlotly({})
  output$save_zone_count <- renderUI({})
  output$Frequency <- renderPlotly({})
  output$Voltage <- renderPlotly({})
  output$distance_response <- renderPlotly({})
  output$save_distance_response <- renderUI({})
  output$map <- renderPlotly({})

  }

reset_data_cleaning_tab <- function(input, output, session, stringsAsFactors) {
  output$circuit_details_editor <- renderDT({})
  output$site_details_editor <- renderDT({})
  output$site_plot <- renderPlotly({})
}

#' Check pre-event interval value is in data and matches time window
#' @param pre_event_interval The pre-event timestamp to be checked
#' @param load_start_time The start boundary timestamp for loaded data
#' @param load_start_time The end boundary timestamp for loaded data
#' @param window_length The input window length
#' @param data The dataframe containing the loaded data
validate_pre_event_interval <- function(pre_event_interval, load_start_time, load_end_time, window_length, data){
  logdebug("Validating pre_event_interval", logger="shinyapp")
  # check pre-event interval is on the selected time offset
  error_check_passed = TRUE
  start_time <- as.POSIXct(load_start_time, tz = "Australia/Brisbane")
  end_time <- as.POSIXct(load_end_time, tz = "Australia/Brisbane")
  data_at_set_pre_event_interval = filter(data, ts == pre_event_interval)
  if (dim(data_at_set_pre_event_interval)[1] == 0) {
    long_error_message <- c("The pre-event time interval does not match any time step in the time series data.")
    long_error_message <- paste(long_error_message, collapse = '')
    shinyalert("Error in pre-event time interval", long_error_message)
    error_check_passed = FALSE
  }
  # check pre-event interval is inside the time window loaded
  if ((pre_event_interval < start_time) | (pre_event_interval > end_time)) {
    long_error_message <- c("The pre-event interval must be within the time window of loaded data.")
    long_error_message <- paste(long_error_message, collapse = '')
    shinyalert("Error in pre-event time interval", long_error_message)
    error_check_passed = FALSE
  }
  # check window length does not extend outside time window loaded.
  if ((pre_event_interval + window_length * 60) > end_time) {
    long_error_message <- c("The event window length must be within the time window of loaded data.")
    long_error_message <- paste(long_error_message, collapse = '')
    shinyalert("Error in pre-event time interval", long_error_message)
    error_check_passed = FALSE
  }
  return(error_check_passed)
}

#' Get the ideal frequency-watt response for a region
#' @param frequency_data The frequency data input to the tool
#' @param region_to_load Which region (state) to pull from the frequency data
#' @return Ideal response time series dataframe
ideal_response_from_frequency <- function(frequency_data, region_to_load) {
  if (dim(frequency_data)[1] > 0) {
    temp_f_data <- select(frequency_data, ts, region_to_load) 
    temp_f_data <- setnames(temp_f_data, c(region_to_load), c("f"))
    temp_f_data <- mutate(temp_f_data, f = as.numeric(f))
    ideal_response_to_plot <- ideal_response(temp_f_data)
  } else {
    temp_f_data <- data.frame()
    ideal_response_to_plot <- data.frame()
  }
  results <- list()
  results$region_frequency <- temp_f_data
  results$ideal_response_to_plot <- ideal_response_to_plot
  return(results)
}

#' Apply user filters to combined data
filter_combined_data <- function(combined_data, off_grid_postcodes, clean, size_groupings, standards, postcodes,
                                manufacturers, models, sites, circuits) {
  logdebug("Apply user filters to combined data", logger="shinyapp")
  combined_data_f <- combined_data
  site_types <- c("pv_site_net", "pv_site", "pv_inverter_net", "pv_inverter")
  if (length(clean) > 0) {combined_data_f <- filter(combined_data_f, clean %in% clean)}
  combined_data_f <- filter(combined_data_f, sum_ac<=100)
  if (length(site_types) > 0 ) {combined_data_f <- filter(combined_data_f, con_type %in% site_types)}
  if (length(off_grid_postcodes) > 0 ) {
    combined_data_f <- filter(combined_data_f, !(s_postcode %in% off_grid_postcodes))}
  if (length(size_groupings) > 0 ) {combined_data_f <- filter(combined_data_f, Grouping %in% size_groupings)}
  if (length(standards) > 0 ) {combined_data_f <- filter(combined_data_f, Standard_Version %in% standards)}
  if (length(postcodes) > 0) {combined_data_f <- filter(combined_data_f, s_postcode %in% postcodes)}
  if (length(manufacturers) > 0) {combined_data_f <- filter(combined_data_f, manufacturer %in% manufacturers)}
  if (length(models) > 0) {combined_data_f <- filter(combined_data_f, model %in% models)}
  if (length(sites) > 0) {combined_data_f <- filter(combined_data_f, site_id %in% sites)}
  if (length(circuits) > 0) {combined_data_f <- filter(combined_data_f, c_id %in% circuits)}
  return(combined_data_f)
}

#' Check for UFLS disconnections
#' Runs UFLS status checks - both voltage and timestamp based.
#' @seealso
#' * [ufls_detection_tstamp()] for detail of timestamp based UFLS detection
#' * [ufls_detection_voltage()] for detail of voltage based UFLS detection
#' @return An updated dataframe with columns for UFLS check results
ufls_detection <- function(
  db_interface, combined_data_f, region_to_load, pre_event_interval, window_length, pre_event_ufls_window_length,
  post_event_ufls_window_length, pre_event_ufls_stability_threshold, post_event_delay
) {
  logdebug("run ufls detection", logger = "shinyapp")
  ufls_statuses_ts <- ufls_detection_tstamp(db = db_interface, region = region_to_load, 
                                  pre_event_interval = pre_event_interval, 
                                  pre_event_window_length = pre_event_ufls_window_length,
                                  post_event_window_length = post_event_ufls_window_length, 
                                  pre_pct_sample_seconds_threshold = pre_event_ufls_stability_threshold,
                                  post_event_delay = post_event_delay)
  
  ufls_statuses_v <- ufls_detection_voltage(combined_data_f, pre_event_interval, window_length, fill_nans = FALSE)
  combined_data_f <- left_join(combined_data_f, ufls_statuses_ts, by = c("c_id"))
  combined_data_f <- left_join(combined_data_f, ufls_statuses_v, by = c("c_id"))
  combined_data_f <- mutate(combined_data_f, response_category =
                            if_else((ufls_status == "UFLS Dropout") |
                                      (ufls_status_v == "UFLS Dropout"),
                                    "UFLS Dropout", response_category))
  return(combined_data_f)
}

#' Determine distance based zones for each site
#' Distance is from input event location to the postcode the site is in
#' @return An updated dataframe with zones added
determine_distance_zones <- function(
  combined_data_f, postcode_data, event_latitude, event_longitude, zone_one_radius, zone_two_radius, zone_three_radius,
  zones
) {
  if(length(combined_data_f$ts) > 0){
    combined_data_f <- get_distance_from_event(combined_data_f, postcode_data, event_latitude, event_longitude)
    combined_data_f <- get_zones(combined_data_f, zone_one_radius, zone_two_radius, zone_three_radius)
    combined_data_f <- mutate(combined_data_f,  zone=ifelse(zone %in% c(NA), "NA", zone))
    if (length(zones) < 3) { combined_data_f <- filter(combined_data_f, zone %in% zones)}
  } else {
    logwarn("no timestamp data found for distance zones")
  }
  return(combined_data_f)
}

#' Normalise power by c_id, based on pre-event interval power
#' @return An updated dataframe with the column c_id_norm_power added
normalise_c_id_power_by_pre_event <- function(combined_data_f, pre_event_interval) {
  logdebug('Calc event normalised power', logger="shinyapp")
  event_powers <- filter(combined_data_f, ts == pre_event_interval)
  event_powers <- mutate(event_powers, event_power=power_kW)
  event_powers <- select(event_powers, c_id, clean, event_power)
  combined_data_f <- left_join(combined_data_f, event_powers, by=c("c_id", "clean"))
  combined_data_f <- mutate(combined_data_f, c_id_norm_power=power_kW/event_power)
  return(combined_data_f)
}

#' Normalise power by c_id, based on maximum daily power
#' Optionally adds pre event normalised power as a new column when pre_event_interval is provided
#' @return An updated dataframe with the column c_id_daily_norm_power added
normalise_c_id_power_by_daily_max <- function(combined_data, max_power, pre_event_interval){
  logdebug('Calc daily normalised power', logger="shinyapp")
  combined_data_f <- left_join(combined_data, max_power, by=c("c_id", "clean"))
  combined_data_f <- mutate(combined_data_f, c_id_daily_norm_power=power_kW/max_power)
  if (!missing(pre_event_interval)){
    pre_event_daily_norm_power <- filter(combined_data_f, ts == pre_event_interval)
    pre_event_daily_norm_power <- mutate(pre_event_daily_norm_power, pre_event_norm_power = c_id_daily_norm_power)
    pre_event_daily_norm_power <- select(pre_event_daily_norm_power, clean, c_id, pre_event_norm_power)
    combined_data_f <- left_join(combined_data_f, pre_event_daily_norm_power, by=c("c_id", "clean"))
  }
  return(combined_data_f)
}

determine_performance_factors <- function(combined_data_f, pre_event_interval) {
  logdebug('Calc site peformance factors', logger="shinyapp")
  combined_data_f <- calc_site_performance_factors(combined_data_f)
  combined_data_f <- setnames(combined_data_f, c("ts"), c("Time"))
  
  combined_data_f <- event_normalised_power(combined_data_f, pre_event_interval, keep_site_id=TRUE)
  combined_data_f <- setnames(
    combined_data_f, c("Event_Normalised_Power_kW"), c("Site_Event_Normalised_Power_kW"))
  combined_data_f <- setnames(combined_data_f, c("Time"), c("ts"))
  return(combined_data_f)
}

#' Upscale disconnections
upscale_and_summarise_disconnections <- function(circuit_summary, manufacturer_install_data, load_date, region_to_load, exclude_solar_edge) {
  logdebug('Summarise and upscale disconnections on a manufacturer basis.', logger="shinyapp")
  if (exclude_solar_edge){
    circuits_to_summarise <- filter(circuit_summary, manufacturer != "SolarEdge" | 
                                    is.na(manufacturer))
    manufacturer_install_data <- filter(manufacturer_install_data, manufacturer != "SolarEdge" | 
                                          is.na(manufacturer))
  } else {
    circuits_to_summarise <- circuit_summary
    manufacturer_install_data <- manufacturer_install_data
  }
  upscaling_results <- get_upscaling_results(circuits_to_summarise, manufacturer_install_data, load_date, 
                                              region_to_load, sample_threshold = 30)
  upscaling_results$with_separaute_ufls_counts <- get_upscaling_results_excluding_ufls_affected_circuits(
    circuits_to_summarise, manufacturer_install_data, load_date, region_to_load, sample_threshold = 30)

  write.csv(upscaling_results$manufacturers_missing_from_cer, 
            "logging/manufacturers_missing_from_cer.csv", row.names=FALSE)
  write.csv(upscaling_results$manufacturers_missing_from_input_db, 
            "logging/manufacturers_missing_from_input_db.csv", row.names=FALSE)

  return(upscaling_results)
}

check_grouping <- function(settings) {
  if (settings$standard_agg==FALSE & settings$pst_agg==FALSE & settings$grouping_agg==FALSE &
      settings$manufacturer_agg==FALSE & settings$model_agg==FALSE & settings$zone_agg==FALSE &
      settings$circuit_agg==TRUE & settings$compliance_agg==TRUE & settings$reconnection_compliance_agg){
    no_grouping=TRUE
  } else {
    no_grouping=FALSE
  }
  return(no_grouping)
}

run_analysis <- function(data, settings) {
  error_check_passed = validate_pre_event_interval(
    settings$pre_event_interval,
    settings$load_start_time,
    settings$load_end_time,
    settings$window_length,
    data$combined_data
  )

  if (error_check_passed) {
    logging::logdebug("error checks passed", logger="shinyapp")
    id <- showNotification("Updating plots", duration=1000)

    response_data <- ideal_response_from_frequency(
      data$frequency_data, settings$region_to_load
    )
    data$ideal_response_to_plot <- response_data$ideal_response_to_plot
    data$region_frequency <- response_data$region_frequency

    # -------- filter combined data by user filters --------
    combined_data_f <- filter_combined_data(
      data$combined_data, data$off_grid_postcodes, settings$clean, settings$size_groupings, settings$standards,
      settings$postcodes, settings$manufacturers, settings$models, settings$sites, settings$circuits
    )

    if(length(combined_data_f$ts) > 0){
      # -------- categorise response --------
      combined_data_f <- categorise_response(
        combined_data_f, settings$pre_event_interval, settings$window_length, settings$NED_threshold)
      combined_data_f <- mutate(combined_data_f,  
                                response_category = ifelse(response_category %in% c(NA), "NA", response_category))
      combined_data_f <- filter(combined_data_f, response_category %in% settings$responses)

      combined_data_f <- ufls_detection(
        data$db, combined_data_f, settings$region_to_load, settings$pre_event_interval, settings$window_length,
        settings$pre_event_ufls_window_length, settings$post_event_ufls_window_length,
        settings$pre_event_ufls_stability_threshold, as.numeric(settings$duration)
      )

      # process alerts where they exist
      alert_data <- data$db$get_alerts_data()
      if(length(alert_data$c_id) > 0){
        # run islanded site assessment
        combined_data_f <- classify_islands(combined_data_f, alert_data, settings$pre_event_interval,
                                            settings$window_length)
      }
    }

    if ("Islanded" %in% names(combined_data_f) & settings$exclude_islanded_circuits){
      combined_data_f <- filter(combined_data_f, !Islanded)
      combined_data_f <- subset(combined_data_f, select = -c(Islanded, island_assessment, islanding_alert))
    } else if ("Islanded" %in% names(combined_data_f)){
      combined_data_f <- mutate(combined_data_f, response_category=ifelse(island_assessment %in%
              c("Frequency disruption", "Voltage disruption", "Gateway curtailed"), "Undefined", response_category))
    }

    combined_data_f <- determine_distance_zones(
      combined_data_f, data$postcode_data, settings$event_latitude, settings$event_longitude, settings$zone_one_radius,
      settings$zone_two_radius, settings$zone_three_radius, settings$zones
    )

    # -------- filter by time window --------
    logdebug('filter by time window', logger="shinyapp")
    if (length(settings$offsets) < length(data$unique_offsets)) { 
      combined_data_f <- filter(combined_data_f, time_offset %in% settings$offsets)
    }
    
    combined_data_f <- normalise_c_id_power_by_pre_event(combined_data_f, settings$pre_event_interval)
    
    if(length(combined_data_f$ts) > 0){
      id2 <- showNotification("Calculating site performance factors", duration=1000)
      combined_data_f <- determine_performance_factors(combined_data_f, settings$pre_event_interval)

      # -------- determine f-W compliance --------
      if(dim(data$ideal_response_to_plot)[1]>0){
        ideal_response_downsampled <- down_sample_1s(
          data$ideal_response_to_plot, settings$duration, min(combined_data_f$ts))
        data$ideal_response_downsampled <- ideal_response_downsampled
        combined_data_f <- 
          calc_error_metric_and_compliance_2(combined_data_f, 
                                              ideal_response_downsampled,
                                              data$ideal_response_to_plot,
                                              settings$compliance_threshold,
                                              settings$start_buffer,
                                              settings$end_buffer,
                                              settings$end_buffer_responding,
                                              settings$disconnecting_threshold)
        combined_data_f <- mutate(
          combined_data_f,  compliance_status=ifelse(compliance_status %in% c(NA), "NA", compliance_status))
      } else {
        combined_data_f <- mutate(combined_data_f, compliance_status="Undefined")  
      }
      if (length(settings$compliance) < 8) {
        combined_data_f <- filter(combined_data_f, compliance_status %in% settings$compliance)
      }
      # --------
      
      # -------- determine reconnection compliace --------
      logdebug('Set reconnection compliance values', logger="shinyapp")
      max_power <- data$db$get_max_circuit_powers(settings$region_to_load)
      combined_data_f <- normalise_c_id_power_by_daily_max(
        combined_data_f, max_power, settings$pre_event_interval
      )
      event_window_data <- filter(combined_data_f, ts >= settings$pre_event_interval)
      reconnection_categories <- create_reconnection_summary(event_window_data, settings$pre_event_interval,
                                                              settings$disconnecting_threshold,
                                                              reconnect_threshold = settings$reconnection_threshold,
                                                              ramp_rate_threshold = settings$ramp_rate_threshold,
                                                              ramp_threshold_for_compliance = 
                                                                settings$total_ramp_threshold_for_compliance,
                                                              ramp_threshold_for_non_compliance = 
                                                                settings$total_ramp_threshold_for_non_compliance,
                                                              ramp_rate_change_resource_limit_threshold = 
                                                                settings$ramp_rate_change_resource_limit_threshold)
      combined_data_f <- left_join(combined_data_f, reconnection_categories, by = 'c_id')
      removeNotification(id2)
      # --------
      
      # -------- calculate summary stats --------
      # inputs: combined_data_f
      # outputs: v$sample_count_table
      # dependencies: too many to list for  find_grouping_cols()
      # Count samples in each data series to be displayed
      logdebug('Count samples in each data series to be displayed', logger="shinyapp")
      grouping_cols <- find_grouping_cols(settings)
      
      data$sample_count_table <- vector_groupby_count(combined_data_f, grouping_cols)
      if (settings$confidence_category %in% grouping_cols) {
        population_groups <- grouping_cols[settings$confidence_category != grouping_cols]
        population_count_table <- vector_groupby_count(combined_data_f, population_groups)
        population_count_table <- dplyr::rename(population_count_table, sub_population_size=sample_count)
        data$sample_count_table <- left_join(population_count_table, data$sample_count_table, by=population_groups)
        data$sample_count_table$percentage_of_sub_pop <- data$sample_count_table$sample_count / data$sample_count_table$sub_population_size
        data$sample_count_table$percentage_of_sub_pop <- round(data$sample_count_table$percentage_of_sub_pop, digits = 4)
        result <- mapply(ConfidenceInterval, data$sample_count_table$sample_count, 
                          data$sample_count_table$sub_population_size, 0.95)
        data$sample_count_table$lower_95_CI <- round(result[1,], digits = 4)
        data$sample_count_table$upper_95_CI <- round(result[2,], digits = 4)
        data$sample_count_table$width <- data$sample_count_table$upper_95_CI - data$sample_count_table$lower_95_CI
      }
    }
    # --------

    no_grouping <- check_grouping(settings)

    # Procced to  aggregation and plotting only if there is less than 1000 data series to plot, else stop and notify the
    # user.
    logdebug('Proceed to aggregation and plotting', logger="shinyapp")
    if ((sum(data$sample_count_table$sample_count)<1000 & no_grouping) | 
        (length(data$sample_count_table$sample_count)<1000 & !no_grouping)){
      
      # -------- Create copies of data? --------
      # inputs: combined_data_f
      # outputs: v$combined_data_f, combined_data_f2
      # dependencies: raw_upscale()
      if(length(combined_data_f$ts) > 0){
      # Copy data for saving
      logdebug('Copy data for saving', logger="shinyapp")
      combined_data_cols <- c("ts", "site_id", "c_id", "power_kW", "c_id_norm_power", "v", "f", "s_state",
                              "s_postcode", "pv_installation_year_month", "Standard_Version", "Grouping", "sum_ac",
                              "clean", "manufacturer", "model", "site_performance_factor", "response_category",
                              "zone", "distance", "lat", "lon", "e", "con_type", "first_ac", "polarity",
                              "compliance_status", "reconnection_compliance_status",
                              "manual_droop_compliance", "manual_reconnect_compliance", "reconnection_time",
                              "ramp_above_threshold", "c_id_daily_norm_power", "max_power", "ufls_status",
                              "pre_event_sampled_seconds", "post_event_sampled_seconds", "ufls_status_v",
                              "pre_event_v_mean", "post_event_v_mean")
      if("Islanded" %in% names(combined_data_f)){
        combined_data_cols <- append(combined_data_cols, c("Islanded", "island_assessment", "islanding_alert"), 34)
      }
      data$combined_data_f <- combined_data_f[, combined_data_cols]

      # Create copy of filtered data to use in upscaling
      combined_data_f2 <- combined_data_f
        if(settings$raw_upscale){combined_data_f2 <- upscale(combined_data_f2, data$install_data)}
      }
      # -------- 

      # Check that the filter does not result in an empty dataframe.
      logdebug('Check that the filter does not result in an empty dataframe.', logger="shinyapp")
      if(length(combined_data_f$ts) > 0){
        
        # -------- Initialise aggregate dataframes  --------
        # inputs: combined_data_f, combined_data_f2, grouping_cols
        # outputs: agg_norm_power, agg_f_and_v, v$agg_power, v$response_count, v$zone_count, v$distance_response, geo_data, v$circuit_summary
        # dependencies: vector_groupby_norm_power(), vector_groupby_f_and_v(), vector_groupby_power(), vector_groupby_count_response(),
        #   vector_groupby_count_zones(), vector_groupby_cumulative_distance(), vector_groupby_system()
        if (settings$norm_power_filter_off_at_t0){
          combined_data_for_norm_power <- filter(combined_data_f,  response_category != "5 Off at t0")
        } else {
          combined_data_for_norm_power <- combined_data_f
        }
        data$agg_norm_power <- vector_groupby_norm_power(combined_data_for_norm_power, grouping_cols)
        agg_f_and_v <- vector_groupby_f_and_v(combined_data_f, grouping_cols)
        data$agg_power <- vector_groupby_power(combined_data_f2, grouping_cols)
        data$response_count <- vector_groupby_count_response(combined_data_f, grouping_cols)
        data$zone_count <- vector_groupby_count_zones(combined_data_f, grouping_cols)
        data$distance_response <- vector_groupby_cumulative_distance(combined_data_f, grouping_cols)
        data$geo_data <- vector_groupby_system(combined_data_f, grouping_cols)
        data$circuit_summary <- distinct(combined_data_f, c_id, clean, .keep_all = TRUE)
        circ_sum_cols <- c("site_id", "c_id", "s_state", "s_postcode", "pv_installation_year_month",
                            "Standard_Version", "Grouping", "sum_ac", "clean", "manufacturer", "model",
                            "response_category", "zone", "distance", "lat", "lon", "con_type", "first_ac", "polarity",
                            "compliance_status", "reconnection_compliance_status", "manual_droop_compliance",
                            "manual_reconnect_compliance", "reconnection_time", "ramp_above_threshold", "max_power",
                            "ufls_status", "pre_event_sampled_seconds", "post_event_sampled_seconds", "ufls_status_v",
                            "pre_event_v_mean", "post_event_v_mean")
        if("Islanded" %in% names(data$circuit_summary)){
          circ_sum_cols <- append(circ_sum_cols, c("Islanded", "island_assessment", "islanding_alert"), 26)
        }
        data$circuit_summary <- data$circuit_summary[, circ_sum_cols]
        data$circuit_summary$tool_hash <-git2r::revparse_single(revision="HEAD")$sha

        # Combine data sets that have the same grouping so they can be saved in a single file
        if (no_grouping){
          et <- settings$pre_event_interval
          # agg_norm_power <- event_normalised_power(agg_norm_power, et, keep_site_id=TRUE)
          data$agg_power <- left_join(data$agg_power, data$agg_norm_power[, c("c_id_norm_power", "c_id", "Time")], 
                                    by=c("Time", "c_id"))
        } else {
          et <- settings$pre_event_interval
          # agg_norm_power <- event_normalised_power(agg_norm_power, et,  keep_site_id=FALSE)
          data$agg_power <- left_join(data$agg_power, data$agg_norm_power[, c("c_id_norm_power", "series", "Time")], 
                                    by=c("Time", "series"))
        }
        data$agg_power <- left_join(data$agg_power, agg_f_and_v[, c("Time", "series", "Voltage", "Frequency")], 
                                  by=c("Time", "series"))
        # --------
        
        # -------- Create disconnection_sumaries --------
        # inputs: v$circuit_summary, v$manufacturer_install_data, 
        # outputs: circuits_to_summarise, v$disconnection_summary, v$upscaled_disconnection
        # dependencies: load_date(), region_to_load(), exclude_solar_edge()
        # Summarise and upscale disconnections on a manufacturer basis.
        upscaling_results <- upscale_and_summarise_disconnections(
          data$circuit_summary, data$manufacturer_install_data, settings$load_date, settings$region_to_load, settings$exclude_solar_edge
        )
        data$disconnection_summary <- upscaling_results$disconnection_summary
        data$upscaled_disconnections <- upscaling_results$upscaled_disconnections
        data$disconnection_summary_with_separate_ufls_counts <- 
          upscaling_results$with_separate_ufls_counts$disconnection_summary
        data$upscaled_disconnections_with_separate_ufls_counts <- 
          upscaling_results$with_separate_ufls_counts$upscaled_disconnections

        if(length(upscaling_results$manufacturers_missing_from_cer$manufacturer) > 0) {
          long_error_message <- c("Some manufacturers present in the input data could not be ",
                                  "matched to the cer data set. A list of these has been saved in the ",
                                  "file logging/manufacturers_missing_from_cer.csv. You may want to review the ", 
                                  "mapping used in processing the input data.")
          long_error_message <- paste(long_error_message, collapse = '')
          shinyalert("Manufacturers missing from CER data", long_error_message)
        }
        
        if(length(upscaling_results$manufacturers_missing_from_input_db$manufacturer) > 0) {
          long_error_message <- c("Some manufacturers present in the CER data could not be ",
                                  "matched to the input data set. A list of these has been saved in the ",
                                  "file logging/manufacturers_missing_from_input_db.csv. You may wish to review the ", 
                                  "file to check the number and names of missing manufacturers. ")
          long_error_message <- paste(long_error_message, collapse = '')
          shinyalert("Manufacturers missing from input data", long_error_message)
        }
      }
    }
  }
  return(data)
}


server <- function(input,output,session){
  # Create radio button dyamically so label can be updated
  output$duration <- renderUI({radioButtons("duration", label=strong("Sampled duration (seconds), select one."), 
                                            choices = list("5","30","60"), 
                                            selected = "60", inline = TRUE)})
  # Hide these inputs by default, they are shown once data is loaded.
  hide("frequency_data")
  hide("choose_frequency_data")
  hide("region_to_load")
  hide("duration")
  hide("load_first_filter_settings")
  hide("perform_clean")
  hide("keep_raw")
  hide("load_data")
  hide("standard_agg")
  hide("raw_upscale")
  hide("pst_agg")
  hide("grouping_agg")
  hide("response_agg")
  hide("manufacturer_agg")
  hide("model_agg")
  hide("circuit_agg")
  hide("zone_agg")
  hide("compliance_agg")
  hide("reconnection_compliance_agg")
  hide("save_settings")
  hide("load_second_filter_settings")
  hide("norm_power_filter_off_at_t0")
  hide("confidence_category")
  options(DT.options = list(pageLength = 3))
  # Get input from GUI
  settings_file <- reactive({input$settings_file})
  database_name <- reactive({input$database_name})
  frequency_data_file <- reactive({input$frequency_data})
  region_to_load <- reactive({input$region_to_load})
  duration <- reactive({input$duration})
  standards <- reactive({input$StdVersion})
  responses <- reactive({input$responses})
  postcodes <- reactive({input$postcodes})
  manual_compliance_type <- reactive({input$manual_compliance_type})
  compliance_circuits <- reactive({input$compliance_circuits})
  compliance_cleaned_or_raw <- reactive({input$compliance_cleaned_or_raw})
  set_c_id_compliance <- reactive({input$set_c_id_compliance})
  manufacturers <- reactive({input$manufacturers})
  models <- reactive({input$models})
  sites <- reactive({input$sites})
  circuits <- reactive({input$circuits})
  zones <- reactive({input$zones})
  compliance <- reactive({input$compliance})
  reconnection_compliance <- reactive({input$reconnection_compliance})
  offsets <- reactive({input$offsets})
  size_groupings <- reactive({input$size_groupings})
  clean <- reactive({input$cleaned})
  raw_upscale <- reactive({input$raw_upscale})
  pst_agg <- reactive({input$pst_agg})
  grouping_agg <- reactive({input$grouping_agg})
  response_agg <- reactive({input$response_agg})
  manufacturer_agg <- reactive({input$manufacturer_agg})
  perform_clean <- reactive({input$perform_clean})
  keep_raw <- reactive({input$keep_raw})
  model_agg <- reactive({input$model_agg})
  circuit_agg <- reactive({input$circuit_agg})
  zone_agg <- reactive({input$zone_agg})
  compliance_agg <- reactive({input$compliance_agg})
  reconnection_compliance_agg <- reactive({input$reconnection_compliance_agg})
  load_date <- reactive({
    date_as_str <- as.character(input$load_date[1])
  })
  load_start_time <- reactive({
    date_as_str <- as.character(input$load_date[1])
    time_as_str <- substr(input$load_time_start, 12,19)
    start_time_as_str <- paste(date_as_str, time_as_str)
  })
  load_end_time <- reactive({
    date_as_str <- as.character(input$load_date[2])
    time_as_str <- substr(input$load_time_end, 12,19)
    end_time_as_str <- paste(date_as_str, time_as_str)
  })
  start_time <- reactive({
    date_as_str <- as.character(input$date[1])
    time_as_str <- substr(input$time_start, 12,19)
    start_time_as_str <- paste(date_as_str, time_as_str)
    start_date_time <- strptime(start_time_as_str, format="%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")
    start_date_time
  })
  end_time <- reactive({
    date_as_str <- as.character(input$date[2])
    time_as_str <- substr(input$time_end, 12,19)
    end_time_as_str <- paste(date_as_str, time_as_str)
    end_date_time <- strptime(end_time_as_str, format="%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")
    end_date_time
  })
  pre_event_interval <- reactive({
    date_as_str <- as.character(input$event_date)
    time_as_str <- substr(input$pre_event_interval, 12, 19)
    date_time_as_str <- paste(date_as_str, time_as_str)
    pre_event_interval_date_time <- strptime(date_time_as_str, format="%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")
    pre_event_interval_date_time
  })
  agg_on_standard <- reactive({input$standard_agg})
  window_length <- reactive({input$window_length})
  post_event_ufls_window_length <- reactive({input$post_event_ufls_window_length})
  event_latitude <- reactive({input$event_latitude})
  event_longitude <- reactive({input$event_longitude})
  zone_one_radius <- reactive({input$zone_one_radius})
  zone_two_radius <- reactive({input$zone_two_radius})
  zone_three_radius <- reactive({input$zone_three_radius})
  norm_power_filter_off_at_t0 <- reactive({input$norm_power_filter_off_at_t0})
  confidence_category <- reactive({input$confidence_category})
  
  # Values from settings tab
  compliance_threshold <- reactive({input$compliance_threshold})
  start_buffer <- reactive({input$start_buffer})
  end_buffer <- reactive({input$end_buffer})
  end_buffer_responding <- reactive({input$end_buffer_responding})
  reconnection_threshold <- reactive({input$reconnection_threshold})
  reconnection_time_threshold_for_compliance <- reactive({input$reconnection_time_threshold_for_compliance})
  ramp_rate_threshold <- reactive({input$ramp_rate_threshold})
  total_ramp_threshold_for_compliance <- reactive({input$total_ramp_threshold_for_compliance})
  total_ramp_threshold_for_non_compliance <- reactive({input$total_ramp_threshold_for_non_compliance})
  ramp_rate_change_resource_limit_threshold <- reactive({input$ramp_rate_change_resource_limit_threshold})
  pre_event_ufls_window_length <- reactive({input$pre_event_ufls_window_length})
  pre_event_ufls_stability_threshold <- reactive({input$pre_event_ufls_stability_threshold})
  NED_threshold <- reactive({input$NED_threshold})
  disconnecting_threshold <- reactive({input$disconnecting_threshold})
  exclude_solar_edge <- reactive({input$exclude_solar_edge})
  exclude_islanded_circuits <- reactive({input$exclude_islanded_circuits})
  
  
  # Store the main data table in a reactive value so it is accessable outside 
  # the observe event that creates it.
  v <- reactiveValues(combined_data = data.frame(),
                      combined_data_no_ac_filter = data.frame(),
                      agg_power = data.frame(),
                      agg_norm_power = data.frame(),
                      install_data = data.frame(),
                      site_details = data.frame(),
                      circuit_details = data.frame(),
                      circuit_details_for_editing = data.frame(),
                      site_details_raw = data.frame(),
                      site_details_for_editing = data.frame(),
                      proxy_site_details_editor = 1,
                      proxy_circuit_details_editor = 1,
                      combined_data_after_clean = data.frame(),
                      time_series_data = data.frame(),
                      sample_count_table = data.frame(),
                      combined_data_f = data.frame(),
                      performance_factors = data.frame(),
                      postcode_data = data.frame(),
                      response_count = data.frame(),
                      zone_count = data.frame(),
                      distance_response = data.frame(),
                      frequency_data = data.frame(),
                      unique_offsets = c(),
                      circuit_summary = data.frame(),
                      region_frequency = data.frame(),
                      trigger_update_manual_compliance_tab = FALSE
                      )
  
  observeEvent(input$connect_to_database, {
    v$db <- DBInterface$new()
    
    good_connection = FALSE
    tryCatch({
      v$db$connect_to_existing_database(database_name())
      good_connection = TRUE
      },
      error =  function(cond) {
        shinyalert("An error occured while connecting to the database.", cond$message)
    })
    
    if (good_connection) {
      min_timestamp <- v$db$get_min_timestamp()
      max_timestamp <- v$db$get_max_timestamp()
      
      output$load_time_start <- renderUI({
        timeInput("load_time_start", label = strong('Enter start time'), 
                  value = min_timestamp)
      })
      output$load_time_end <- renderUI({
        timeInput("load_time_end", label = strong('Enter end time'), 
                  value = max_timestamp)
      })
      
      output$load_date <- renderUI({
        dateRangeInput("load_date", label = strong('Date range (yyyy-mm-dd):'),
                       start = strftime(min_timestamp, format = "%Y-%m-%d"),
                       end = strftime(min_timestamp, format = "%Y-%m-%d"),
                       min = strftime(min_timestamp, format = "%Y-%m-%d"),
                       max = strftime(max_timestamp, format = "%Y-%m-%d"),
                       startview = "year")
      })
      
      shinyjs::show("frequency_data")
      shinyjs::show("choose_frequency_data")
      shinyjs::show("region_to_load")
      shinyjs::show("duration")
      shinyjs::show("load_data")
      shinyjs::show("load_first_filter_settings")
    }
  })
  
  # This is the event that runs when the "Load data" button on the GUI is
  # Clicked. 
  observeEvent(input$load_data, {
    id <- showNotification("Loading data", duration = 1000)
    error_check_passed = TRUE
    
    # Peform error checking before loading data.
    if (is.na(as.character(input$load_date[1])) | is.na(as.character(input$load_date[2]))) {
      shinyalert("Please provide a valid start and end date.", '')
      error_check_passed = FALSE
    } else {
      start_time <- format(as.POSIXct(load_start_time(), tz = "Australia/Brisbane"), tz = 'GMT')
      end_time <- format(as.POSIXct(load_end_time(), tz = "Australia/Brisbane"), tz = 'GMT')
      if (difftime(end_time, start_time, units = 'hours') > 2) {
        long_error_message <- c("A time window of greater than 2 hours was provided, it might take a while to load.")
        long_error_message <- paste(long_error_message, collapse = '')
        shinyalert("Warning long time window", long_error_message)
      }
      if (!(load_end_time() > load_start_time())) {
        long_error_message <- c("The start time must be before the end time.")
        long_error_message <- paste(long_error_message, collapse = '')
        shinyalert("Error in time window", long_error_message)
        error_check_passed = FALSE
      }
    }
    if(frequency_data_file() != '') {
      v$frequency_data <- read.csv(file=frequency_data_file(), header=TRUE, stringsAsFactors = FALSE)
      expected_columns_names <- c("ts", "QLD", "NSW", "VIC", "SA", "TAS", "WA")
      column_names <- names(v$frequency_data)
      for (col_name in column_names){ 
        if (!(col_name %in% expected_columns_names)){
          long_error_message <- c("The frequency data csv should only contain the following columns, ", 
                                  "ts, QLD, NSW, VIC, SA, TAS, WA. If this error persists after checking ",
                                  "the columns names, then the file may be incorrectly incoded, please ", 
                                  "re-save it using the CSV (Comma delimited) (*.csv) option in excel.")
          long_error_message <- paste(long_error_message, collapse = '')
          shinyalert("Error loading frequency data", long_error_message)
          error_check_passed = FALSE
        }
      }
      if (!(region_to_load() %in% column_names)){
        long_error_message <- c("The frequency data csv must contain a column for the region being loaded. ", 
                                "Either choose to load no frequency data or ensure it containts data for the ",
                                "selected region.")
        long_error_message <- paste(long_error_message, collapse = '')
        shinyalert("Error loading frequency data", long_error_message)
        error_check_passed = FALSE
      }
    }
    
    install_data_file <- "inbuilt_data/cer_cumulative_capacity_and_number.csv"
    if (!file.exists(install_data_file)){
      long_error_message <- c("The required file cer_cumulative_capacity_and_number.csv could ",
                              "not be found. Please add it to the inbuilt_data directory.")
      long_error_message <- paste(long_error_message, collapse = '')
      shinyalert("Error loading install data", long_error_message)
      error_check_passed = FALSE
    }
    time_series_data <- v$db$get_filtered_time_series_data(state = region_to_load(), duration = duration(), 
                                                           start_time = start_time, end_time = end_time)
    if (dim(time_series_data)[1] == 0){
      long_error_message <- c("The region and duration selected resulted in an empty dataset, please try another selection")
      long_error_message <- paste(long_error_message, collapse = '')
      shinyalert("Error loading timeseries data", long_error_message)
      error_check_passed = FALSE
    }
    cer_manufacturer_data <- "inbuilt_data/cer_cumulative_capacity_and_number_by_manufacturer.csv"
    if (!file.exists(cer_manufacturer_data)){
      long_error_message <- c("The required file cer_manufacturer_data could ",
                              "not be found. Please add it to the inbuilt_data directory.")
      long_error_message <- paste(long_error_message, collapse = '')
      shinyalert("Error loading manufacturer install data", long_error_message)
      error_check_passed = FALSE
    }
    off_grid_postcodes <- "inbuilt_data/off_grid_postcodes.csv"
    if (!file.exists(off_grid_postcodes)){
      long_error_message <- c("The required file off_grid_postcodes could ",
                              "not be found. Please add it to the inbuilt_data directory.")
      long_error_message <- paste(long_error_message, collapse = '')
      shinyalert("Error loading off grid post code data", long_error_message)
      error_check_passed = FALSE
    }

      if (error_check_passed){
        site_details_raw <- v$db$get_site_details_raw()
        site_details_raw <- process_raw_site_details(site_details_raw)
        v$site_details <- mutate(site_details_raw, clean = 'raw')
        
        if (v$db$check_if_table_exists('site_details_cleaned')){
          site_details_clean <- v$db$get_site_details_cleaned()
          site_details_clean <- process_raw_site_details(site_details_clean)
          site_details_clean <- mutate(site_details_clean, clean = 'clean')
          v$site_details <- bind_rows(v$site_details, site_details_clean)
          
          v$site_details_for_editing <- v$db$get_site_details_cleaning_report()
          v$site_details_for_editing <- filter(v$site_details_for_editing, s_state == region_to_load())
          output$site_details_editor <- renderDT(isolate(v$site_details_for_editing), selection='single', rownames=FALSE, 
                                                 editable=TRUE)
          v$proxy_site_details_editor <- dataTableProxy('site_details_editor')
        }
        
        v$site_details <- site_categorisation(v$site_details)
        v$site_details <- size_grouping(v$site_details)

        circuit_details_raw <- v$db$get_circuit_details_raw()
        v$circuit_details <- mutate(circuit_details_raw, clean = 'raw')
        v$circuit_details_raw <- v$circuit_details
        
        if (v$db$check_if_table_exists('circuit_details_cleaned')){
          circuit_details_clean <- v$db$get_circuit_details_cleaned()
          circuit_details_clean <- mutate(circuit_details_clean, clean = 'clean')
          v$circuit_details <- bind_rows(v$circuit_details, circuit_details_clean)
          
          v$circuit_details_for_editing <- v$db$get_circuit_details_cleaning_report()
          v$circuit_details_for_editing <- filter(v$circuit_details_for_editing, site_id %in% v$site_details_for_editing$site_id)
          
          output$circuit_details_editor <- renderDT(isolate(v$circuit_details_for_editing), selection='single', 
                                                    rownames=FALSE, editable=TRUE)
          v$proxy_circuit_details_editor <- dataTableProxy('circuit_details_editor')
        }
  
        time_series_data <- process_time_series_data(time_series_data)
  
        v$combined_data <- inner_join(time_series_data, v$circuit_details, by = "c_id")
        v$combined_data <- inner_join(v$combined_data, v$site_details, by = c("site_id", "clean"))
        
        v$combined_data <- perform_power_calculations(v$combined_data)
        
        removeNotification(id)
  
  
        # Load in the install data from CSV.
        install_data <- read.csv(file = install_data_file, header = TRUE, stringsAsFactors = FALSE)
        v$install_data <- process_install_data(install_data)
        
        manufacturer_install_data <- read.csv(file = cer_manufacturer_data, header = TRUE, stringsAsFactors = FALSE)
        v$manufacturer_install_data <- calc_installed_capacity_by_standard_and_manufacturer(manufacturer_install_data)
        
        postcode_data_file <- "inbuilt_data/PostcodesLatLongQGIS.csv"
        postcode_data <- read.csv(file=postcode_data_file, header=TRUE, stringsAsFactors = FALSE)
        v$postcode_data <- process_postcode_data(postcode_data)
        
        off_grid_postcodes <- "inbuilt_data/off_grid_postcodes.csv"
        v$off_grid_postcodes <- read.csv(file=off_grid_postcodes, header=TRUE, stringsAsFactors = FALSE)
        v$off_grid_postcodes <- v$off_grid_postcodes$postcodes
        
        if(frequency_data_file()!=''){
          v$frequency_data <- read.csv(file=frequency_data_file(), header=TRUE, stringsAsFactors = FALSE)
          v$frequency_data <- mutate(v$frequency_data, 
                                     ts=as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")))
        } else {
          v$frequency_data <- data.frame()
        }
        
        # Get offset filter options and label
        v$combined_data <- get_time_offsets(v$combined_data)
        v$unique_offsets <- get_time_series_unique_offsets(v$combined_data)
        sample_counts <- get_offset_sample_counts(v$combined_data, v$unique_offsets)
        unique_offsets_filter_label <- make_offset_filter_label(sample_counts, v$unique_offsets)
        
        
        # Filtering option widgets are rendered after the data is loaded, this is 
        # because they are only usable once there is data to filter. Additionally
        # The data loaded can then be used to create the appropraite options for 
        # filtering.
        output$postcodes <- renderUI({
          selectizeInput("postcodes", label=strong("Select postcodes"), choices = sort(unique(v$site_details$s_postcode)), 
                        multiple=TRUE)  
          })
        output$manufacturers <- renderUI({
          selectizeInput("manufacturers", label=strong("Select manufacturers"), 
                         choices = sort(unique(v$site_details$manufacturer)), multiple=TRUE)  
        })
        output$models <- renderUI({
          selectizeInput("models", label=strong("Select models"), choices = sort(unique(v$site_details$model)), multiple=TRUE)  
        })
        output$sites <- renderUI({
          selectizeInput("sites", label=strong("Select Sites"), choices = sort(unique(v$site_details$site_id)), multiple=TRUE)  
        })
        output$circuits <- renderUI({
          selectizeInput("circuits", label=strong("Select Circuits"), choices = sort(unique(v$circuit_details$c_id)), 
                         multiple=TRUE)  
        })
        shinyjs::show("standard_agg")
        output$size_groupings <- renderUI({
          checkboxGroupButtons(inputId="size_groupings", label=strong("Size Groupings"),
                               choices=list("30-100kW", "<30 kW"), selected=list("30-100kW", "<30 kW"),
                               justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"),no=icon("remove", lib="glyphicon")),
                               direction = "vertical")
        })
        output$cleaned <- renderUI({
          checkboxGroupButtons(inputId="cleaned", 
                               label=strong("Data sets"), choices=list("clean", "raw"),
                               selected=list("clean"),
                               justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")),
                               direction = "vertical")
        })
        output$StdVersion <- renderUI({
          checkboxGroupButtons(inputId="StdVersion", 
                               label=strong("AS47777 Version:"), choices=list("AS4777.3:2005", "Transition", 
                                                                              "AS4777.2:2015", "AS4777.2:2015 VDRT"),
                               selected=list("AS4777.3:2005", "Transition", "AS4777.2:2015", "AS4777.2:2015 VDRT"),
                               justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")),
                               direction = "vertical")
          })
        output$responses <- renderUI({
          checkboxGroupButtons(inputId="responses", 
                               label=strong("Select Responses:"),
                               choices=list("1 Ride Through", "2 Curtail", "3 Drop to Zero", "4 Disconnect","5 Off at t0", 
                                            "6 Not enough data", "7 UFLS Dropout", "Undefined", NA),
                               selected=list("1 Ride Through", "2 Curtail", "3 Drop to Zero", "4 Disconnect",
                                             "5 Off at t0", "6 Not enough data", "7 UFLS Dropout", "Undefined", NA),
                               justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")),
                               direction = "vertical")
        })
        output$zones <- renderUI({
          checkboxGroupButtons(inputId="zones", label=strong("Zones"), 
                               choices=list("1 Zone", "2 Zone", "3 Zone", "Undefined", NA),
                               selected=list("1 Zone", "2 Zone", "3 Zone", "Undefined", NA), 
                               justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")),
                               direction = "vertical")
        })
        output$compliance <- renderUI({
          checkboxGroupButtons(inputId="compliance", label=strong("Compliance"), 
                               choices=list("Compliant", "Non-compliant Responding", 
                                            "Non-compliant", "UFLS Dropout", "Disconnect/Drop to Zero",
                                            "Off at t0", "Not enough data", "Undefined", NA),
                               selected=list("Compliant", "Non-compliant Responding", 
                                             "Non-compliant", "UFLS Dropout", "Disconnect/Drop to Zero",
                                             "Off at t0", "Not enough data", "Undefined", NA), 
                               justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")),
                               direction = "vertical")
        })
        output$reconnection_compliance <- renderUI({
          checkboxGroupButtons(inputId="reconnection_compliance", label=strong("Reconnection Compliance"), 
                               choices=list("Compliant", "Non Compliant", 
                                            "Unsure", "Cannot be set", NA),
                               selected=list("Compliant", "Non Compliant", 
                                             "Unsure", "Cannot be set", NA),
                               justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")),
                               direction = "vertical")
        })
        output$offsets <- renderUI({
          checkboxGroupButtons(inputId="offsets", label=unique_offsets_filter_label, 
                               choices=v$unique_offsets, selected=c(v$unique_offsets[which.max(sample_counts)]) ,
                               justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")),
                               direction = "vertical")
        })
        shinyjs::show("raw_upscale")
        shinyjs::show("pst_agg")
        shinyjs::show("grouping_agg")
        shinyjs::show("grouping_agg")
        shinyjs::show("manufacturer_agg")
        shinyjs::show("response_agg")
        shinyjs::show("circuit_agg")
        shinyjs::show("zone_agg")
        shinyjs::show("compliance_agg")
        shinyjs::show("reconnection_compliance_agg")
        shinyjs::show("save_settings")
        shinyjs::show("load_second_filter_settings")
        shinyjs::show("norm_power_filter_off_at_t0")
        shinyjs::show("confidence_category")
        output$event_date <- renderUI({
          dateInput("event_date", label=strong('Event date (yyyy-mm-dd):'), 
                    value=strftime(floor_date(get_mode(v$combined_data$ts), "day"), format="%Y-%m-%d"), startview="year")
        })
        output$pre_event_interval <- renderUI({
          timeInput("pre_event_interval", label=strong('Pre-event time interval (Needs to match exactly to data timestamp)'), 
                    value = as.POSIXct("12:13:55",format="%H:%M:%S"))
        })
        output$window_length <- renderUI({
          numericInput("window_length", label=strong('Set window length (min),
                                                     Only data in this window is used for response analysis.'), 
                       value=5, min = 1, max = 100, step = 1)
        })
        output$post_event_ufls_window_length <- renderUI({
          numericInput("post_event_ufls_window_length", label=strong('Set post event UFLS window length (min)'), 
                       value=5, min = 1, max = 100, step = 1)
        })
        output$event_latitude <- renderUI({
          numericInput("event_latitude", label=strong('Set event latitude'), value=-28.838132)
        })
        output$event_longitude <- renderUI({
          numericInput("event_longitude", label=strong('Set event longitude'), value=151.096832)
        })
        output$zone_one_radius <- renderUI({
          numericInput("zone_one_radius", label=strong('Set zone one outer radius (km)'), value=200)
        })
        output$zone_two_radius <- renderUI({
          numericInput("zone_two_radius", label=strong('Set zone two outer radius (km)'), value=600)
        })
        output$zone_three_radius <- renderUI({
          numericInput("zone_three_radius", label=strong('Set zone three outer radius (km)'), value=1000)
        })
        output$update_plots <- renderUI({
          actionButton("update_plots", "Update plots")
        })
      }
    removeNotification(id)
  })

  # Create plots when update plots button is clicked.
  observeEvent(input$update_plots, {
    logdebug("update_plots event triggered", logger="shinyapp")

    data <- reactiveValuesToList(v)
    settings <- get_current_settings()
    data <- run_analysis(data, settings)
    for (d_name in names(data)) {
      v[[d_name]] <- data[[d_name]]
    }

    no_grouping <- check_grouping(settings)

    if ((sum(v$sample_count_table$sample_count)<1000 & no_grouping) | 
      (length(v$sample_count_table$sample_count)<1000 & !no_grouping)){
      if(length(v$combined_data_f$ts) > 0){
        # Create plots on main tab
        logdebug('create plots', logger="shinyapp")
        
        # -------- Render plots and save buttons --------
        # inputs:v$agg_power,  v$sample_count_table, ideal_response_to_plot, agg_norm_power, v$response_count, v$zone_count, v$agg_power, v$distance_response, geo_data, v$combined_data_f
        # outputs: output$...
        # dependencies: event_longitude(), event_latitude(), zone_one_radius(), pre_event_interval(), duration()
        output$PlotlyTest <- renderPlotly({
          plot_ly(v$agg_power, x=~Time, y=~Power_kW, color=~series, type="scattergl")  %>% 
            layout(yaxis = list(title = "Aggregate Power (kW)"))})
        output$save_agg_power <- renderUI({
          shinySaveButton("save_agg_power", "Save Aggregated Results", "Save file as ...", filetype=list(xlsx="csv"))
          })
        output$save_underlying <- renderUI({
          shinySaveButton("save_underlying", "Save Underlying Data", "Save file as ...", filetype=list(xlsx="csv"))
          })
        output$save_circuit_summary <- renderUI({
          shinySaveButton("save_circuit_summary", "Save Circuit Summary", "Save file as ...", filetype=list(xlsx="csv"))
        })

        output$batch_save <- renderUI({
          shinySaveButton("batch_save", "Batch save", "Save file as ...", filetype=list(xlsx="csv"))
        })
        output$save_ideal_response <- renderUI({
          shinySaveButton("save_ideal_response", "Save response", "Choose directory for report files ...", 
                          filetype=list(xlsx="csv"))
        })
        output$save_ideal_response_downsampled <- renderUI({
          shinySaveButton("save_ideal_response_downsampled", "Save downsampled response", 
                          "Choose directory for report files ...", filetype=list(xlsx="csv"))
        })
        output$save_manufacturer_disconnection_summary <- renderUI({
          shinySaveButton("save_manufacturer_disconnection_summary", "Save manufacturer disconnection summary", 
                          "Choose directory for report files ...", filetype=list(xlsx="csv"))
        })
        output$save_manufacturer_disconnection_summary_with_separate_ufls_counts <- renderUI({
          shinySaveButton("save_manufacturer_disconnection_summary_with_separate_ufls_counts", 
                          "Save manufacturer disconnection summary with separate ufls counts", 
                          "Choose directory for report files ...", filetype=list(xlsx="csv"))
        })
        output$save_upscaled_disconnection_summary <- renderUI({
          shinySaveButton("save_upscaled_disconnection_summary", "Save upscaled disconnection summary", 
                          "Choose directory for report files ...", filetype=list(xlsx="csv"))
        })
        output$save_upscaled_disconnection_summary_with_separate_ufls_counts <- renderUI({
          shinySaveButton("save_upscaled_disconnection_summary_with_separate_ufls_counts", 
                          "Save upscaled disconnection summary with separate ufls counts", 
                          "Choose directory for report files ...", filetype=list(xlsx="csv"))
        })
      
    
        if ("width" %in% names(v$sample_count_table)) {
          sample_count_table <- datatable(v$sample_count_table) %>% formatStyle(
            "width",  background = styleColorBar(c(0, 1), 'red'))
        } else {
          sample_count_table <- v$sample_count_table
        }
        
        output$sample_count_table <- renderDataTable({sample_count_table})

        output$save_sample_count <- renderUI({shinySaveButton("save_sample_count", "Save data", "Save file as ...", 
                                                              filetype=list(xlsx="csv"))
        })
        if(dim(v$ideal_response_to_plot)[1]>0){
          output$NormPower <- renderPlotly({
            plot_ly(v$agg_norm_power, x=~Time, y=~c_id_norm_power, color=~series, type="scattergl") %>% 
              add_trace(x=~v$ideal_response_to_plot$ts, y=~v$ideal_response_to_plot$norm_power, name='Ideal Response', 
                        mode='markers', inherit=FALSE) %>%
              add_trace(x=~v$ideal_response_downsampled$time_group, y=~v$ideal_response_downsampled$norm_power, 
                        name='Ideal Response Downsampled', mode='markers', inherit=FALSE) %>%
              layout(yaxis=list(title="Circuit power normalised to value of pre-event interval, \n aggregated by averaging"))
          })
        } else {
          output$NormPower <- renderPlotly({
            plot_ly(v$agg_norm_power, x=~Time, y=~c_id_norm_power, color=~series, type="scattergl", 
                    mode = 'lines+markers') %>% 
              layout(yaxis=list(title="Circuit power normalised to value of pre-event interval, \n aggregated by averaging"))
          }) 
        }
        output$ResponseCount <- renderPlotly({
          plot_ly(v$response_count, x=~series_x, y=~sample_count, color=~series_y, type="bar") %>% 
            layout(yaxis = list(title = 'Fraction of circuits \n (denominator is count post filtering)'),
                    xaxis = list(title = 'Response categories'),
                    barmode = 'stack')
          })
        output$save_response_count <- renderUI({
          shinySaveButton("save_response_count", "Save data", "Save file as ...", filetype=list(xlsx="csv"))
          })
        
        output$ZoneCount <- renderPlotly({
          plot_ly(v$zone_count, x=~series_x, y=~sample_count, color=~series_y, type="bar") %>%
            layout(yaxis = list(title = 'Fraction of zone circuits \n (denominator is count post filtering)'),
                    xaxis = list(title = 'Zone categories'), barmode = 'stack')
          })
        output$save_zone_count <- renderUI({
          shinySaveButton("save_zone_count", "Save data", "Save file as ...", filetype=list(xlsx="csv"))
          })
        if(dim(v$frequency_data)[1]>0){
          output$Frequency <- renderPlotly({
            plot_ly(v$agg_power, x=~Time, y=~Frequency, color=~series, type="scattergl")%>% 
              add_trace(x=~v$region_frequency$ts, y=~v$region_frequency$f, name='High Speed Data', 
                        mode='markers', inherit=FALSE) %>% 
              layout(yaxis=list(title="Average frequency (Hz)"))
            })
        } else {
          output$Frequency <- renderPlotly({
            plot_ly(v$agg_power, x=~Time, y=~Frequency, color=~series, type="scattergl")%>% 
              layout(yaxis=list(title="Average frequency (Hz)"))
          })
        }
        output$Voltage <- renderPlotly({plot_ly(v$agg_power, x=~Time, y=~Voltage, color=~series, type="scattergl") %>% 
            layout(yaxis=list(title="Average volatge (V)"))
          })
        output$distance_response <- renderPlotly({
          plot_ly(v$distance_response, x=~distance, y=~percentage, color=~series, type="scattergl") %>% 
            layout(yaxis=list(title="Cumlative  disconnects / Cumulative circuits \n (Includes response categories 3 and 4)"),
                    xaxis=list(title="Distance from event (km)"))
          })
        output$save_distance_response <- renderUI({
          shinySaveButton("save_distance_response", "Save data", "Save file as ...", filetype=list(xlsx="csv"))
          })
        z1 <- data.frame(circle.polygon(event_longitude(), event_latitude(), zone_one_radius(), sides = 20, units='km', poly.type = "gc.earth"))
        z2 <- data.frame(circle.polygon(event_longitude(), event_latitude(), zone_two_radius(), sides = 20, units='km', poly.type = "gc.earth"))
        z3 <- data.frame(circle.polygon(event_longitude(), event_latitude(), zone_three_radius(), sides = 20, units='km', poly.type = "gc.earth"))
        output$map <- renderPlotly({plot_geo(v$geo_data, lat=~lat, lon=~lon, color=~percentage_disconnect) %>%
            add_polygons(x=~z1$lon, y=~z1$lat, inherit=FALSE, fillcolor='transparent', 
                          line=list(width=2,color="grey"), hoverinfo = "none", showlegend=FALSE) %>%
            add_polygons(x=~z2$lon, y=~z2$lat, inherit=FALSE, fillcolor='transparent', 
                          line=list(width=2,color="grey"), hoverinfo = "none", showlegend=FALSE) %>%
            add_polygons(x=~z3$lon, y=~z3$lat, inherit=FALSE, fillcolor='transparent', 
                          line=list(width=2,color="grey"), hoverinfo = "none", showlegend=FALSE) %>%
            add_markers(x=~v$geo_data$lon, y=~v$geo_data$lat,  inherit=FALSE, 
                        hovertext=~v$geo_data$info, legendgroup = list(title = "Percentage Disconnects"),
                        marker=list(color=~percentage_disconnect, colorbar=list(title='Percentage \n Disconnects'), 
                                    colorscale='Bluered')) %>%
            layout(annotations = 
                      list(x = 1, y = -0.1, text = "Note: pecentage disconnects includes categories 3 and 4.", 
                          showarrow = F, xref='paper', yref='paper', 
                          xanchor='right', yanchor='auto', xshift=0, yshift=0))
          })
        
        output$compliance_cleaned_or_raw <- 
          renderUI({radioButtons("compliance_cleaned_or_raw", 
                                  label=strong("Choose data set"), 
                                  choices = list("clean","raw"), 
                                  selected = v$combined_data_f$clean[1], 
                                  inline = TRUE)})
        
        v$reconnection_profile <- create_reconnection_profile(pre_event_interval(), ramp_length_minutes = 6,
                                                              time_step_seconds = as.numeric(duration()))
        
        removeNotification(id)
        # --------
      } else {
        # If there is no data left after filtering alert the user and create an empty plot.
        shinyalert("Opps", "There is no data to plot")
        reset_chart_area(input, output, session)
        removeNotification(id)
      }
    } else {
      shinyalert("Wow", "You are trying to plot more than 1000 series, maybe try
                  narrowing down those filters and agg settings")
      reset_chart_area(input, output, session)
      removeNotification(id)
    }
    logdebug('Update plots completed', logger="shinyapp")
  })
  
  observeEvent(input$compliance_cleaned_or_raw, {
                 
    if(compliance_cleaned_or_raw() %in% v$combined_data_f$clean) {
      # Setting up manual compliance tab.
      circuit_options <- filter(v$combined_data_f, clean == compliance_cleaned_or_raw())
      set.seed(002)
      v$c_id_vector <- sample(unique(circuit_options$c_id))
      set.seed(NULL)
      v$compliance_counter <- 1
      message <- paste0("Select circuit (now viewing circuit ", v$compliance_counter, ' of ', length(v$c_id_vector) ,")")
      circuit_to_view <- v$c_id_vector[[v$compliance_counter]]
      output$compliance_circuits <- renderUI({selectizeInput("compliance_circuits", label=strong(message), choices=as.list(v$c_id_vector), 
                                                          multiple=FALSE, selected=circuit_to_view)})
      output$get_next_c_id <- renderUI({actionButton("get_next_c_id", "Next")})
      output$get_previous_c_id <- renderUI({actionButton("get_previous_c_id", "Previous")})
    } else {
      output$compliance_cleaned_or_raw <- renderUI({radioButtons("compliance_cleaned_or_raw", 
                                                                 label=strong("Choose data set"), 
                                                                 choices = list("clean","raw"), 
                                                                 selected = v$combined_data_f$clean[1], 
                                                                 inline = TRUE)})
    }
    
    output$manual_compliance_type <- renderUI({radioButtons("manual_compliance_type", 
                                                            label = strong("Compliance types"), 
                                                            choices = list("Over frequency","Reconnection"), 
                                                            selected = "Over frequency", inline = TRUE)})
    
  })

  observeEvent(c(input$compliance_circuits, input$manual_compliance_type), {
    if (compliance_circuits() %in% v$c_id_vector){
      v$compliance_counter <- match(c(compliance_circuits()), v$c_id_vector)
      message <- paste0("Select circuit (now viewing circuit ", v$compliance_counter, ' of ', length(v$c_id_vector) ,")")
      circuit_to_view <- v$c_id_vector[[v$compliance_counter]]
      output$compliance_circuits <- isolate(renderUI({selectizeInput("compliance_circuits", label=strong(message), 
                                                                     choices=as.list(v$c_id_vector), 
                                                                     multiple=FALSE, selected=circuit_to_view)}))
      data_to_view <- filter(filter(v$combined_data_f, clean==compliance_cleaned_or_raw()), c_id==compliance_circuits())
      
      if (manual_compliance_type() == "Over frequency"){
        
        output$set_c_id_compliance <- renderUI({radioButtons("set_c_id_compliance", label = strong("Compliance"), 
                                                              choices = list("Not set","Compliant","Non-compliant", 
                                                                             "Non-compliant Responding", "Disconnect", 
                                                                             "Unsure"), 
                                                             selected = data_to_view$manual_droop_compliance[1], inline = TRUE)})
        if (compliance_cleaned_or_raw() == 'clean'){
          circuit_data <- filter(v$circuit_details_for_editing, c_id == compliance_circuits())
          updateRadioButtons(session, "set_c_id_compliance", 
                             selected = circuit_data$manual_droop_compliance[1])
        } else {
          circuit_data <- filter(v$circuit_details_raw, c_id == compliance_circuits())
          updateRadioButtons(session, "set_c_id_compliance", 
                             selected = circuit_data$manual_droop_compliance[1])
          
        }
        
      } else {
        
        output$set_c_id_compliance <- renderUI({radioButtons("set_c_id_compliance", label = strong("Compliance"), 
                                                             choices = list("Not set","Compliant","Too Fast", 
                                                                            "Too Slow", "Unsure"), 
                                                             selected = data_to_view$manual_reconnect_compliance[1], inline = TRUE)})
        
        if (compliance_cleaned_or_raw() == 'clean'){
          circuit_data <- filter(v$circuit_details_for_editing, c_id == compliance_circuits())
          updateRadioButtons(session, "set_c_id_compliance", 
                             selected = circuit_data$manual_reconnect_compliance[1])
        } else {
          circuit_data <- filter(v$circuit_details_raw, c_id == compliance_circuits())
          updateRadioButtons(session, "set_c_id_compliance", 
                             selected = circuit_data$manual_reconnect_compliance[1])
          
        }
        
      }
      

      data_to_view <- mutate(data_to_view, Time=ts)
      if (manual_compliance_type() == "Over frequency"){
        
        if(dim(v$ideal_response_to_plot)[1]>0){
          output$compliance_plot <- renderPlotly({
            plot_ly(data_to_view, x=~Time, y=~c_id_norm_power, type="scatter") %>% 
              add_trace(x=v$ideal_response_to_plot$ts, y=v$ideal_response_to_plot$norm_power, name='Ideal Response', 
                        mode='markers', inherit=FALSE) %>%
              add_trace(x=v$ideal_response_downsampled$time_group, y=v$ideal_response_downsampled$norm_power, 
                        name='Ideal Response Downsampled', mode='markers', inherit=FALSE) %>%
              layout(yaxis=list(title="Circuit power \n normalised to value of pre-event interval"))
          })
        } else {
          output$compliance_plot <- renderPlotly({
            plot_ly(data_to_view, x=~Time, y=~c_id_norm_power, type="scatter") %>% 
              layout(yaxis=list(title="Circuit power \n normalised to value of pre-event interval"))
          })
        }
        
      } else {
        
        output$compliance_plot <- renderPlotly({
          plot_ly(data_to_view, x=~Time, y=~c_id_daily_norm_power, type="scatter") %>% 
            add_trace(x=v$reconnection_profile$ts, y=v$reconnection_profile$norm_power, name='Ideal Response', 
                      mode='markers', inherit=FALSE) %>%
            layout(yaxis=list(title="Circuit power \n normalised to max circuit power"))
        })
        
      }

    } else {
      shinyalert("Wow", "That circuit id does not exist.")
    }
  })
  
  observeEvent(input$set_c_id_compliance, {
    current_c_id <- v$c_id_vector[[v$compliance_counter]]
    if (compliance_cleaned_or_raw() == "clean"){
      if (manual_compliance_type() == "Over frequency"){
        v$circuit_details_for_editing <- mutate(v$circuit_details_for_editing, 
                                  manual_droop_compliance=
                                    ifelse((c_id==current_c_id),
                                           set_c_id_compliance(),
                                           manual_droop_compliance))
      } else {
        v$circuit_details_for_editing <- mutate(v$circuit_details_for_editing, 
                                                manual_reconnect_compliance=
                                                  ifelse((c_id==current_c_id),
                                                         set_c_id_compliance(),
                                                         manual_reconnect_compliance))
        
      }
      v$db$update_circuit_details_cleaned(v$circuit_details_for_editing)
    } else {
      if (manual_compliance_type() == "Over frequency"){
        v$circuit_details_raw <- mutate(v$circuit_details_raw, 
                                    manual_droop_compliance=
                                      ifelse((c_id==current_c_id),
                                             set_c_id_compliance(),
                                             manual_droop_compliance))
      } else {
        v$circuit_details_raw <- mutate(v$circuit_details_raw, 
                                        manual_reconnect_compliance=
                                          ifelse((c_id==current_c_id),
                                                 set_c_id_compliance(),
                                                 manual_reconnect_compliance))
      }
      v$db$update_circuit_details_raw(v$circuit_details_raw)
    }
  })
  
  observeEvent(input$get_next_c_id,{
    if (v$compliance_counter < length(v$c_id_vector)){
      v$compliance_counter <- v$compliance_counter + 1
      circuit_to_view <- v$c_id_vector[[v$compliance_counter]]
      message <- paste0("Select circuit (now viewing circuit ", v$compliance_counter, ' of ', length(v$c_id_vector) ,")")
      output$compliance_circuits <- renderUI({selectizeInput("compliance_circuits", label=strong(message), choices=as.list(v$c_id_vector), 
                                                          multiple=FALSE, selected=circuit_to_view)})      
    }
  })
  
  observeEvent(input$get_previous_c_id,{
    if (v$compliance_counter > 1){
      v$compliance_counter <- v$compliance_counter - 1
      circuit_to_view <- v$c_id_vector[[v$compliance_counter]]
      message <- paste0("Select circuit (now viewing circuit ", v$compliance_counter, ' of ', length(v$c_id_vector) ,")")
      output$compliance_circuits <- renderUI({selectizeInput("compliance_circuits", label=strong(message), choices=as.list(v$c_id_vector), 
                                                          multiple=FALSE, selected=circuit_to_view)})      
    }
  })
  
  # Save data from aggregate pv power plot
  observeEvent(input$save_agg_power,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_agg_power", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_agg_power)
    if (nrow(fileinfo) > 0) {
      write.csv(v$agg_power, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  # Save underlying data by circuit id and time stamp
  observeEvent(input$save_underlying,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_underlying", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_underlying)
    if (nrow(fileinfo) > 0) {
      id <- showNotification("Saving Underlying Data at Site Level", duration=1000)
      write.csv(v$combined_data_f, as.character(fileinfo$datapath), row.names=FALSE)
      removeNotification(id)
    }
  })
  
  # Save circuit summary
  observeEvent(input$save_circuit_summary,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_circuit_summary", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_circuit_summary)
    if (nrow(fileinfo) > 0) {
      id <- showNotification("Saving Circuit Summary", duration=1000)
      write.csv(v$circuit_summary, as.character(fileinfo$datapath), row.names=FALSE)
      removeNotification(id)
    }
  })
  
  # Save data from sample count table
  observeEvent(input$save_sample_count,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_sample_count", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_sample_count)
    if (nrow(fileinfo) > 0) {write.csv(v$sample_count_table, as.character(fileinfo$datapath), row.names=FALSE)}
  })
  
  # Save data on aggregated response categories
  observeEvent(input$save_response_count,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_response_count", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_response_count)
    if (nrow(fileinfo) > 0) {write.csv(v$response_count, as.character(fileinfo$datapath), row.names=FALSE)}
  })
  
  # Save data on zones
  observeEvent(input$save_zone_count, {
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_zone_count", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_zone_count)
    if (nrow(fileinfo) > 0) {write.csv(v$zone_count, as.character(fileinfo$datapath), row.names=FALSE)}
  })
  
  # Save data on response percentage by distance
  observeEvent(input$save_distance_response, {
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_distance_response", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_distance_response)
    if (nrow(fileinfo) > 0) {write.csv(v$distance_response, as.character(fileinfo$datapath), row.names=FALSE)}
  })
  
  # Save ideal response curve
  observeEvent(input$save_ideal_response,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_ideal_response", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_ideal_response)
    if (nrow(fileinfo) > 0) {
      write.csv(v$ideal_response_to_plot, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  
  # Save downsampled ideal response curve
  observeEvent(input$save_ideal_response_downsampled,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_ideal_response_downsampled", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_ideal_response_downsampled)
    if (nrow(fileinfo) > 0) {
      write.csv(v$ideal_response_downsampled, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  
  observeEvent(input$save_manufacturer_disconnection_summary,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_manufacturer_disconnection_summary", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_manufacturer_disconnection_summary)
    if (nrow(fileinfo) > 0) {
      write.csv(v$disconnection_summary, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  
  observeEvent(input$save_manufacturer_disconnection_summary_with_separate_ufls_counts,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_manufacturer_disconnection_summary_with_separate_ufls_counts", roots=volumes, 
                  session=session)
    fileinfo <- parseSavePath(volumes, input$save_manufacturer_disconnection_summary_with_separate_ufls_counts)
    if (nrow(fileinfo) > 0) {
      write.csv(v$disconnection_summary_with_separate_ufls_counts, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  
  observeEvent(input$save_upscaled_disconnection_summary,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_upscaled_disconnection_summary", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_upscaled_disconnection_summary)
    if (nrow(fileinfo) > 0) {
      write.csv(v$upscaled_disconnections, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  
  observeEvent(input$save_upscaled_disconnection_summary_with_separate_ufls_counts,{
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_upscaled_disconnection_summary_with_separate_ufls_counts", roots=volumes, 
                  session=session)
    fileinfo <- parseSavePath(volumes, input$save_upscaled_disconnection_summary_with_separate_ufls_counts)
    if (nrow(fileinfo) > 0) {
      write.csv(v$upscaled_disconnections_with_separate_ufls_counts, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  
  get_current_settings <- function(){
    settings <- vector(mode='list')
    
    settings$database_name <- database_name()
    
    settings$region_to_load <- region_to_load()
    settings$duration <- duration()
    settings$load_start_time <- load_start_time()
    settings$load_end_time <- load_end_time()
    settings$frequency_data_file <- frequency_data_file()
    
    settings$cleaned <- clean()
    settings$standards <- standards()
    settings$responses <- responses()
    settings$postcodes <- postcodes()
    settings$manufactures <- manufacturers()
    settings$models <- models()
    settings$sites <- sites()
    settings$circuits <- circuits()
    settings$zones <- zones()
    settings$compliance <- compliance()
    settings$reconnection_compliance <- reconnection_compliance()
    settings$offsets <- offsets()
    settings$size_groupings <- size_groupings()
    
    settings$standard_agg <- agg_on_standard()
    settings$pst_agg <- pst_agg()
    settings$grouping_agg <- grouping_agg()
    settings$response_agg <- response_agg()
    settings$manufacturer_agg <- manufacturer_agg()
    settings$model_agg <- model_agg()
    settings$circuit_agg <- circuit_agg()
    settings$zone_agg <- zone_agg()
    settings$reconnection_compliance_agg <- reconnection_compliance_agg()
    settings$compliance_agg <- compliance_agg()
    
    settings$confidence_category <- confidence_category()
    settings$raw_upscale <- raw_upscale()
    settings$load_date <- load_date()
    
    settings$pre_event_interval <- pre_event_interval()
    settings$window_length <- window_length()
    settings$post_event_ufls_window_length <- post_event_ufls_window_length()
    settings$event_latitude <- event_latitude()
    settings$event_longitude <- event_longitude()
    settings$zone_one_radius <- zone_one_radius()
    settings$zone_two_radius <- zone_two_radius()
    settings$zone_three_radius <- zone_three_radius()
    settings$zone_three_radius <- zone_three_radius()
    settings$norm_power_filter_off_at_t0 <- norm_power_filter_off_at_t0()
  
    settings$compliance_threshold <- compliance_threshold()
    settings$start_buffer <- start_buffer()
    settings$end_buffer <- end_buffer()
    settings$end_buffer_responding <- end_buffer_responding()
    settings$reconnection_threshold <- reconnection_threshold()
    settings$reconnection_time_threshold_for_compliance <- reconnection_time_threshold_for_compliance()
    settings$ramp_rate_threshold <- ramp_rate_threshold()
    settings$total_ramp_threshold_for_compliance <- total_ramp_threshold_for_compliance()
    settings$total_ramp_threshold_for_non_compliance <- total_ramp_threshold_for_non_compliance()
    settings$ramp_rate_change_resource_limit_threshold <- ramp_rate_change_resource_limit_threshold()
    settings$pre_event_ufls_window_length <- pre_event_ufls_window_length()
    settings$pre_event_ufls_stability_threshold <- pre_event_ufls_stability_threshold()
    settings$NED_threshold <- NED_threshold()
    settings$disconnecting_threshold <- disconnecting_threshold()
    settings$exclude_solar_edge <- exclude_solar_edge()
    settings$exclude_islanded_circuits <- exclude_islanded_circuits()
    
    return(settings)
  }

  get_settings_as_json <- function(){
    settings <- get_current_settings()
    settings$pre_event_interval <- as.character(settings$pre_event_interval)
    settings_as_json <- toJSON(settings, indent = 1)
    return(settings_as_json)
  }
  
  observeEvent(input$save_settings, {
    settings_as_json <- get_settings_as_json()
    volumes <- c(home=getwd())
    shinyFileSave(input, "save_settings", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_settings)
    if (nrow(fileinfo) > 0) {
      write(settings_as_json, as.character(fileinfo$datapath))
    }
  })
  
  load_settings <- function(){
    settings <- c()
    tryCatch(
      {
        settings <- fromJSON(file = settings_file())
      },
      error = function(cond){
        shinyalert("Opps", "Something went wrong loading the settings, please see the console for more details.")
      }
    )
    return(settings)
  }
  
  observeEvent(input$load_file_from_settings, {
    settings <- load_settings()
    if (length(settings) > 0){updateTextInput(session, "database_name", value = settings$database_name)}
  })
  
  observeEvent(input$load_first_filter_settings, {
    settings <- load_settings()
    if (length(settings) > 0){
      updateDateRangeInput(session, "load_date", start = strftime(settings$load_start_time, format="%Y-%m-%d"))
      updateDateRangeInput(session, "load_date", end = strftime(settings$load_end_time, format="%Y-%m-%d"))
      updateTimeInput(session, "load_time_start", value = settings$load_start_time)
      updateTimeInput(session, "load_time_end", value = settings$load_end_time)
      updateRadioButtons(session, "duration", selected = settings$duration)
      updateRadioButtons(session, "region_to_load", selected = settings$region_to_load)
      updateTextInput(session, "frequency_data", value = settings$frequency_data_file)
    }
  })
  
  observeEvent(input$load_second_filter_settings, {
    settings <- load_settings()
    if (length(settings) > 0){
      updateCheckboxGroupButtons(session, "cleaned", selected = settings$cleaned)
      updateCheckboxGroupButtons(session, "StdVersion", selected = settings$standards)
      updateCheckboxGroupButtons(session, "responses", selected = settings$responses)
      updateCheckboxGroupButtons(session, "zones", selected = settings$zones)
      updateCheckboxGroupButtons(session, "compliance", selected = settings$compliance)
      if ("reconnection_compliance" %in% names(settings)) {
        updateCheckboxGroupButtons(session, "reconnection_compliance", selected = settings$reconnection_compliance)
      }
      updateCheckboxGroupButtons(session, "offsets", selected = settings$offsets)
      updateCheckboxGroupButtons(session, "size_groupings", selected = settings$size_grouping)
      updateSelectizeInput(session, "postcodes", selected = settings$postcodes)
      updateSelectizeInput(session, "manufacturers", selected = settings$manufactures)
      updateSelectizeInput(session, "models", selected = settings$models)
      updateSelectizeInput(session, "sites", selected = settings$sites)
      updateSelectizeInput(session, "circuits", selected = settings$circuits)
      
      updateMaterialSwitch(session, "standard_agg", value = settings$standard_agg)
      updateMaterialSwitch(session, "pst_agg", value = settings$pst_agg)
      updateMaterialSwitch(session, "grouping_agg", value = settings$grouping_agg)
      updateMaterialSwitch(session, "response_agg", value = settings$response_agg)
      updateMaterialSwitch(session, "manufacturer_agg", value = settings$manufacturer_agg)
      updateMaterialSwitch(session, "model_agg", value = settings$model_agg)
      updateMaterialSwitch(session, "circuit_agg", value = settings$circuit_agg)
      updateMaterialSwitch(session, "zone_agg", value = settings$zone_agg)
      updateMaterialSwitch(session, "compliance_agg", value = settings$compliance_agg)
      if ("reconnection_compliance_agg" %in% names(settings)) {
        updateMaterialSwitch(session, "reconnection_compliance_agg", value = settings$reconnection_compliance_agg)
      }
      
      updateRadioButtons(session, "confidence_category", selected = settings$confidence_category)
      updateMaterialSwitch(session, "raw_upscale", value = settings$raw_upscale)
      
      updateDateInput(session, "event_date", value = strftime(settings$pre_event_interval, format = "%Y-%m-%d"))
      updateTimeInput(session, "pre_event_interval", value = settings$pre_event_interval)
      updateNumericInput(session, "window_length", value = settings$window_length)
      updateNumericInput(session, "post_event_ufls_window_length", 
                         value = settings$post_event_ufls_window_length)
      updateNumericInput(session, "event_latitude", value = settings$event_latitude)
      updateNumericInput(session, "event_longitude", value = settings$event_longitude)
      updateNumericInput(session, "zone_one_radius", value = settings$zone_one_radius)
      updateNumericInput(session, "zone_two_radius", value = settings$zone_two_radius)
      updateNumericInput(session, "zone_three_radius", value = settings$zone_three_radius)
    }
  
  })
  
  observeEvent(input$load_backend_settings, {
    settings <- load_settings()
    if (length(settings) > 0){
      updateNumericInput(session, "compliance_threshold", value = settings$compliance_threshold)
      updateNumericInput(session, "start_buffer", value = settings$start_buffer)
      updateNumericInput(session, "end_buffer", value = settings$end_buffer)
      updateNumericInput(session, "end_buffer_responding", value = settings$end_buffer_responding)
      updateNumericInput(session, "reconnection_threshold", value = settings$reconnection_threshold)
      updateNumericInput(session, "reconnection_time_threshold_for_compliance", value = settings$reconnection_time_threshold_for_compliance)
      updateNumericInput(session, "ramp_rate_threshold", value = settings$ramp_rate_threshold)
      updateNumericInput(session, "total_ramp_threshold_for_compliance", value = settings$total_ramp_threshold_for_compliance)
      updateNumericInput(session, "total_ramp_threshold_for_non_compliance", value = settings$total_ramp_threshold_for_non_compliance)
      updateNumericInput(session, "ramp_rate_change_resource_limit_threshold", value = settings$ramp_rate_change_resource_limit_threshold)
      updateNumericInput(session, "pre_event_ufls_window_length", value = settings$pre_event_ufls_window_length)
      updateNumericInput(session, "pre_event_ufls_stability_threshold", 
                         value = settings$pre_event_ufls_stability_threshold)
      updateNumericInput(session,"NED_threshold", value = settings$NED_threshold)
      updateNumericInput(session, "disconnecting_threshold", value = settings$disconnecting_threshold)
      updateMaterialSwitch(session, "exclude_solar_edge", value = settings$exclude_solar_edge)
      updateMaterialSwitch(session, "exclude_islanded_circuits", value = settings$exclude_islanded_circuits)
    }
  })
  
  observe({
    volumes <- c(home=getwd())
    shinyFileChoose(input, "load_settings", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$load_settings)
    if (nrow(fileinfo) > 0) {updateTextInput(session, "settings_file", value=as.character(fileinfo$datapath))}
  })

  # Save data from aggregate pv power plot
  observeEvent(input$batch_save, {
    meta_data <- get_settings_as_json()
    volumes <- getVolumes()
    shinyFileSave(input, "batch_save", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$batch_save)
    if (nrow(fileinfo) > 0) {
      id <- showNotification("Doing batch save", duration=1000)
      datapath <- strsplit(fileinfo$datapath, '[.]')[[1]][1]
      write.csv(v$combined_data_f, paste0(datapath, '_underlying.csv'), row.names=FALSE)
      write.csv(v$circuit_summary, paste0(datapath, '_circ_sum.csv'), row.names=FALSE)
      write(meta_data, paste0(datapath, '_meta_data.json'))
      removeNotification(id)
    }
  })
  
  
  # Time series file selection pop up.
  observe({
    volumes <- c(home=getwd())
    shinyFileChoose(input, "choose_database", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_database)
    if (nrow(fileinfo) > 0) {updateTextInput(session, "database_name", value=as.character(fileinfo$datapath))}
  })
  
  # frequency file selection pop up.
  observe({
    volumes <- c(home=getwd())
    shinyFileChoose(input, "choose_frequency_data", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_frequency_data)
    if (nrow(fileinfo) > 0) {updateTextInput(session, "frequency_data", value=as.character(fileinfo$datapath))}
  })
  
  # Inforce mutual exclusivity of Aggregation settings
  observe({
    if(manufacturer_agg() | model_agg() | pst_agg() | circuit_agg() | circuit_agg() | response_agg() | zone_agg()
       | compliance_agg() | reconnection_compliance_agg() | grouping_agg()){
      updateMaterialSwitch(session=session, "raw_upscale", value = FALSE)
    }
  })
  
  # Inforce mutual exclusivity of Aggregation settings
  observe({
    if(raw_upscale()){
      updateMaterialSwitch(session=session, "manufacturer_agg", value = FALSE)
      updateMaterialSwitch(session=session, "model_agg", value = FALSE)
      updateMaterialSwitch(session=session, "pst_agg", value = FALSE)
      updateMaterialSwitch(session=session, "circuit_agg", value = FALSE)
      updateMaterialSwitch(session=session, "zone_agg", value = FALSE)
      updateMaterialSwitch(session=session, "compliance_agg", value = FALSE)
      updateMaterialSwitch(session=session, "reconnection_compliance_agg", value = FALSE)
      updateMaterialSwitch(session=session, "response_agg", value = FALSE)
      updateMaterialSwitch(session=session, "grouping_agg", value = FALSE)
    }
  })
  
  # Plot the data for the circuit selected in the table
  observeEvent(input$circuit_details_editor_rows_selected, {
    v$proxy_site_details_editor %>% selectRows(NULL)
    if (length(input$circuit_details_editor_rows_selected==1)) {
      c_id_to_plot <- v$circuit_details_for_editing$c_id[input$circuit_details_editor_rows_selected]
      data_to_view <- filter(v$combined_data, c_id==c_id_to_plot)
      output$site_plot <- renderPlotly({plot_ly(data_to_view, x=~ts, y=~power_kW, type="scattergl")})
    }
    
  })
  
  # Plot the data for the site selected in the table
  observeEvent(input$site_details_editor_rows_selected, {
    v$proxy_circuit_details_editor %>% selectRows(NULL)
    if (length(input$site_details_editor_rows_selected==1)) {
      site_id_to_plot <- v$site_details_for_editing$site_id[input$site_details_editor_rows_selected]
      circuits <- filter(v$circuit_details_for_editing, site_id==site_id_to_plot)
      circuits <- unique(circuits$c_id)
      data_to_view <- filter(v$combined_data, c_id %in% circuits)
      data_to_view <- mutate(data_to_view, c_id=as.character(c_id))
      output$site_plot <- renderPlotly({plot_ly(data_to_view, x=~ts, y=~power_kW, color=~c_id, type="scattergl")})
    }
  })
  
  
  # Allow user to edit site details in data cleaning tab
  observeEvent(input$site_details_editor_cell_edit, {
    info = input$site_details_editor_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    value = info$value
    v$site_details_for_editing[i, j] <<- DT::coerceValue(value, v$site_details_for_editing[i, j])
    replaceData(v$proxy_site_details_editor, v$site_details_for_editing, resetPaging=FALSE, rownames=FALSE)  # important
    v$db$update_site_details_cleaned(v$site_details_for_editing)
  })
  
  # Allow user to edit circuit details in data cleaning tab
  observeEvent(input$circuit_details_editor_cell_edit, {
    info = input$circuit_details_editor_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    value = info$value
    v$circuit_details_for_editing[i, j] <<- DT::coerceValue(value, v$circuit_details_for_editing[i, j])
    replaceData(v$proxy_circuit_details_editor, v$circuit_details_for_editing, resetPaging=FALSE, rownames=FALSE) # important
    v$db$update_circuit_details_cleaned(v$circuit_details_for_editing)
  })
}

shinyApp(ui = ui, server = server)
