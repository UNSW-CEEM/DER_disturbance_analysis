##RSHINY APP
library (shiny)
library(shinyTime)
library(shinyWidgets)
library(shinyalert)
library(plotly)
library(feather)
library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(shinycssloaders)
library(shinyFiles)
library(shinyjs)
library(stringr)
library(fasttime)
library(DT)
library(suncalc)
library(ggmap)
library(measurements)
library(assertthat)
library(geosphere)
library(swfscMisc)
library(padr)
library(sqldf)
library(gridExtra)
source("data_manipulation_functions.R")
source("aggregate_functions.R")
source("upscale_function.R")
source("data_cleaning_functions.R")
source("normalised_power_function.R")
source("response_categorisation_function.R")
source("distance_from_event.R")
source("documentation.R")
source("ideal_response_functions.R")
source("create_report_files.R")
source("create_report_tables.R")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  # Allows for the use of notifications.
  useShinyjs(),
  titlePanel("PV System Disturbance Analysis"),
  # Input Bar
  tabsetPanel(
    tabPanel("Main", fluid=TRUE,
      sidebarLayout(
        sidebarPanel(id= "side_panel",
          h4("File selection"),
          textInput("time_series", "Time series file",
                    value="C:/Users/NGorman/Documents/GitHub/DER_disturbance_analysis/data/20200131/20200131_data_SA.feather"
          ),
          shinyFilesButton("choose_ts", "Choose File", 
                      "Select timeseries data file ...", multiple=FALSE
          ),
          HTML("<br><br>"),
          textInput("circuit_details", "Circuit details file",
                    value="C:/Users/NGorman/Documents/GitHub/DER_disturbance_analysis/data/20200131/20200131_circuit_details_nem.csv"
                    ),

          shinyFilesButton("choose_c", "Choose File", "Select circuit details data file ...", multiple=FALSE
          ),
          HTML("<br><br>"),
          textInput("site_details", "Site details file", 
                    value="C:/Users/NGorman/Documents/GitHub/DER_disturbance_analysis/data/20200131/20200131_site_details_nem.csv"
          ),
          shinyFilesButton("choose_site", "Choose File", "Select site details data file ...", multiple=FALSE),
          HTML("<br><br>"),
          textInput("frequency_data", "Frequency data file", 
                    value="C:/Users/NGorman/Documents/GitHub/DER_disturbance_analysis/data/20200131/20200131_Frequency data for analysis.csv"
          ),
          shinyFilesButton("choose_frequency_data", "Choose File", "Select fequency data file ...", multiple=FALSE),
          HTML("<br><br>"),
          radioButtons("region_to_load", label=strong("Regions"), choices = list("All","QLD","NSW", "VIC", "SA", "TAS"), 
                                                    selected = "All", inline = TRUE),
          HTML("<br><br>"),
          uiOutput("duration"),
          materialSwitch(inputId="perform_clean", label=strong("Perform Clean"), status="primary", right=TRUE),
          materialSwitch(inputId="keep_raw", label=strong("Keep pre-cleaned data"), status="primary", right=FALSE),
          actionButton("load_data", "Load data"),
          tags$hr(),
          h4("Time Filter"),
          uiOutput("dateWidget"),
          uiOutput("time_start"),
          uiOutput("time_end"),
          tags$hr(),
          h4("Category Filter"),
          uiOutput("cleaned"),
          uiOutput("region"),
          uiOutput("StdVersion"),
          uiOutput("size_groupings"),
          uiOutput("responses"),
          uiOutput("zones"),
          uiOutput("compliance"),
          uiOutput("postcodes"),
          uiOutput("manufacturers"),
          uiOutput("models"),
          uiOutput("sites"),
          uiOutput("circuits"),
          uiOutput("offsets"),
          tags$hr(),
          h4("Grouping Categories"),
          materialSwitch("Std_Agg_Indiv", label=strong("AS4777:"), status="primary", value=TRUE),
          materialSwitch("grouping_agg", label=strong("Size Grouping:"), status="primary", value=TRUE),
          materialSwitch("response_agg", label=strong("Response Grouping:"), status="primary", value=FALSE),
          materialSwitch("pst_agg", label=strong("Postcodes:"), status="primary", value=FALSE),
          materialSwitch("manufacturer_agg", label=strong("Manufacturer:"),status="primary", value=FALSE),
          materialSwitch("model_agg", label=strong("Models:"), status="primary", value=FALSE),
          materialSwitch("circuit_agg", label=strong("Circuits:"), status="primary", value=FALSE),
          materialSwitch("zone_agg", label=strong("Zones:"), status="primary", value=FALSE),
          materialSwitch("compliance_agg", label=strong("Compliance:"), status="primary", value=FALSE),
          tags$hr(),
          h4("Additional Processing"),
          materialSwitch(inputId="raw_upscale", label=strong("Upscaled Data"), status="primary", right=FALSE),
          tags$hr(),
          h4("Event information"),
          uiOutput("event_date"),
          uiOutput("pre_event_interval"),
          #uiOutput("event_time"),
          uiOutput("window_length"),
          uiOutput("event_latitude"),
          uiOutput("event_longitude"),
          uiOutput("zone_one_radius"),
          uiOutput("zone_two_radius"),
          uiOutput("zone_three_radius"),
          tags$hr(),
          uiOutput("update_plots")
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
          uiOutput("save_report"),
          HTML("<br>"),
          uiOutput("save_ideal_response"),
          HTML("<br>"),
          uiOutput("save_ideal_response_downsampled"),
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
        h4("Editing the tables below changes the data used in the analysis on 
           the main tab, however plots need to be updated for the changes to
           take affect."),
        h4("Cleaned site data (select to view trace)"),
        DTOutput('site_details_editor'),
        h4("Cleaned Circuit data (select to view trace)"),
        DTOutput('circuit_details_editor'),
        HTML("<br><br>"),
        uiOutput("save_cleaned_data")
      )
    ),
    tabPanel("Manual compliance", fluid=TRUE, 
     mainPanel(
       plotlyOutput("compliance_plot"),
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
                            numericInput("compliance_threshold", 
                                         label=strong('Compliance threshold'), 
                                         value=0.5, max=1, min=0),
                            numericInput("start_buffer", 
                                         label=strong('Start buffer, allowed time to reach compliance threshold, in seconds.'), 
                                         value=60),
                            numericInput("end_buffer", 
                                         label=strong('End buffer, allowed time for system ending response early, in seconds.'), 
                                         value=60),
                            numericInput("end_buffer_responding", 
                                         label=strong('Response time, window length for systems to be considered Non Compliant Responding, in seconds.'), 
                                         value=120),
                            numericInput("disconnecting_threshold", 
                                         label=strong('Disconnecting threshold, level below which the ideal response allows disconnecting systems to be considered compliant.'), 
                                         value=0.05, max=1, min=0)
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
  output$dateWidget <- renderUI({})
  output$time_start <- renderUI({})
  output$time_end <- renderUI({})
  output$region <- renderUI({})
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
  output$offsets <- renderUI({}) 
  shinyjs::hide("Std_Agg_Indiv")
  shinyjs::hide("raw_upscale")
  shinyjs::hide("pst_agg")
  shinyjs::hide("grouping_agg")
  shinyjs::hide("grouping_agg")
  shinyjs::hide("manufacturer_agg")
  shinyjs::hide("response_agg")
  shinyjs::hide("circuit_agg")
  shinyjs::hide("zone_agg")
  shinyjs::hide("compliance_agg")
  output$event_date <- renderUI({})
  #output$event_time <- renderUI({})
  output$pre_event_interval <- renderUI({})
  output$window_length <- renderUI({})
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
  output$save_cleaned_data <- renderUI({})
}

server <- function(input,output,session){
  # Create radio button dyamically so label can be updated
  output$duration <- renderUI({radioButtons("duration", label=strong("Sampled duration (seconds), select one. 
                                            Note durations are calculated based on most frequently occuring time between  
                                            measurements for each c_id."), choices = list("5","30","60"), 
                                            selected = "60", inline = TRUE)})
  # Hide these inputs by default, they are shown once data is loaded.
  hide("Std_Agg_Indiv")
  hide("raw_upscale")
  hide("pst_agg")
  hide("grouping_agg")
  hide("response_agg")
  hide("manufacturer_agg")
  hide("model_agg")
  hide("circuit_agg")
  hide("zone_agg")
  hide("compliance_agg")
  options(DT.options = list(pageLength = 3))
  # Get input from GUI
  time_series_file <- reactive({input$time_series})
  circuit_details_file <- reactive({input$circuit_details})
  site_details_file <- reactive({input$site_details})
  frequency_data_file <- reactive({input$frequency_data})
  region_to_load <- reactive({input$region_to_load})
  region <- reactive({input$region})
  duration <- reactive({input$duration})
  standards <- reactive({input$StdVersion})
  responses <- reactive({input$responses})
  postcodes <- reactive({input$postcodes})
  compliance_circuits <- reactive({input$compliance_circuits})
  compliance_cleaned_or_raw <- reactive({input$compliance_cleaned_or_raw})
  set_c_id_compliance <- reactive({input$set_c_id_compliance})
  manufacturers <- reactive({input$manufacturers})
  models <- reactive({input$models})
  sites <- reactive({input$sites})
  circuits <- reactive({input$circuits})
  zones <- reactive({input$zones})
  compliance <- reactive({input$compliance})
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
  # event_time <- reactive({
  #   date_as_str <- as.character(input$event_date)
  #   time_as_str <- substr(input$event_time, 12, 19)
  #   date_time_as_str <- paste(date_as_str, time_as_str)
  #   event_date_time <- strptime(date_time_as_str, format="%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")
  #   event_date_time
  # })
  pre_event_interval <- reactive({
    date_as_str <- as.character(input$event_date)
    time_as_str <- substr(input$pre_event_interval, 12, 19)
    date_time_as_str <- paste(date_as_str, time_as_str)
    pre_event_interval_date_time <- strptime(date_time_as_str, format="%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")
    pre_event_interval_date_time
  })
  agg_on_standard <- reactive({input$Std_Agg_Indiv})
  window_length <- reactive({input$window_length})
  event_latitude <- reactive({input$event_latitude})
  event_longitude <- reactive({input$event_longitude})
  zone_one_radius <- reactive({input$zone_one_radius})
  zone_two_radius <- reactive({input$zone_two_radius})
  zone_three_radius <- reactive({input$zone_three_radius})
  
  # Values from settings tab
  compliance_threshold <- reactive({input$compliance_threshold})
  start_buffer <- reactive({input$start_buffer})
  end_buffer <- reactive({input$end_buffer})
  end_buffer_responding <- reactive({input$end_buffer_responding})
  disconnecting_threshold <- reactive({input$disconnecting_threshold})
  
  
  # Store the main data table in a reactive value so it is accessable outside 
  # the observe event that creates it.
  v <- reactiveValues(combined_data = data.frame(),
                      combined_data_no_ac_filter = data.frame(),
                      agg_power = data.frame(),
                      install_data = data.frame(),
                      site_details = data.frame(),
                      circuit_details = data.frame(),
                      circuit_details_for_editing = data.frame(),
                      site_details_raw = data.frame(),
                      site_details_cleaned = data.frame(),
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
                      circuit_summary = data.frame()
                      )
  
  # This is the event that runs when the "Load data" button on the GUI is
  # Clicked. 
  observeEvent(input$load_data, {
    # Users may enter invalid file paths or attempt to load data that creates
    # an error or warning during the data table combing process. The "tryCatch"
    # function catches these, aborts the loading process and reports the error 
    # to the users. Importantly this prevent the app from crashing.
    #result = tryCatch({
      # Load data from storage.
      # Load site details data.
      if (str_sub(site_details_file(), start=-7)=="feather"){
        # If a feather file is used it is assumed the data is pre-processed.
        # Hence the data is not passed to the raw data processor.
        id <- showNotification("Loading site details from csv", duration=1000)
        v$site_details <- read_feather(site_details_file())
        removeNotification(id)
      }else{
        id <- showNotification("Loading site details from csv", duration=1000)
        sd_data <- read.csv(file=site_details_file(), header=TRUE, stringsAsFactors = FALSE)
        if ('State' %in% colnames(sd_data)) {sd_data <- setnames(sd_data, c("State"), c("s_state"))}
        if ('ac_rating_w'%in% colnames(sd_data)) {sd_data <- mutate(sd_data, ac_rating_w = ac_rating_w/1000)}
        if ('ac_rating_w'%in% colnames(sd_data)) {sd_data <- setnames(sd_data, c("ac_rating_w"), c("ac"))}
        if ('ac_cap_w'%in% colnames(sd_data)) {sd_data <- mutate(sd_data, ac_cap_w = ac_cap_w/1000)}
        if ('ac_cap_w'%in% colnames(sd_data)) {sd_data <- setnames(sd_data, c("ac_cap_w"), c("ac"))}
        if ('dc_cap_w' %in% colnames(sd_data)) {sd_data <- mutate(sd_data, dc_cap_w = dc_cap_w/1000)}
        if ('dc_cap_w' %in% colnames(sd_data)) {sd_data <- setnames(sd_data, c("dc_cap_w"), c("dc"))}
        if ('AC.Rating.kW.' %in% colnames(sd_data)) {sd_data <- setnames(sd_data, c("AC.Rating.kW."), c("ac"))}
        if ('DC.Rating.kW.' %in% colnames(sd_data)) {sd_data <- mutate(sd_data, DC.Rating.kW.=DC.Rating.kW.*1000)}
        if ('DC.Rating.kW.' %in% colnames(sd_data)) {sd_data <- setnames(sd_data, c("DC.Rating.kW."), c("dc"))}
        if ('inverter_manufacturer' %in% colnames(sd_data)) {
          sd_data <- setnames(sd_data, c("inverter_manufacturer"), c("manufacturer"))
        }
        if ('inverter_model' %in% colnames(sd_data)) {sd_data <- setnames(sd_data, c("inverter_model"), c("model"))}
        v$site_details_raw <- sd_data
        v$site_details_raw <- v$site_details_raw %>% mutate(site_id = as.character(site_id))
        # Older site details provided the day of installation not just the month. We 
        # change the name of the column to match the new format which is just by 
        # month but keep the original info regarding the date.
        if("pv_install_date" %in% colnames(v$site_details_raw)){
          v$site_details_raw <- setnames(v$site_details_raw, c("pv_install_date"),
                                         c("pv_installation_year_month"))
        }
        removeNotification(id)
        id <- showNotification("Formatting site details and creating feather cache file", duration=1000)
        # Data from CSV is assumed to need processing.
        v$site_details <- process_raw_site_details(v$site_details_raw)
        # Automatically create a cache of the processed data as a feather file.
        # Allows for much faster data loading for subsequent anaylsis.
        file_no_type = str_sub(site_details_file(), end=-4)
        file_type_feather = paste(file_no_type, "feather", sep="")
        write_feather(v$site_details, file_type_feather)
        removeNotification(id)
      }
      
      # Filter the site details file if All is not selected, this means only the data cleaning for the region selected is peformed.
      if (region_to_load() != 'All'){
        v$site_details <- filter(v$site_details, s_state==region_to_load())
      }
      
      
      # The circuit details file requires no processing and is small so always 
      # load from CSV.
      id <- showNotification("Loading circuit details from csv", duration=1000)
      v$circuit_details <- read.csv(file=circuit_details_file(), header=TRUE, stringsAsFactors = FALSE)
      v$circuit_details <- select(v$circuit_details, c_id, site_id, con_type, polarity)
      v$circuit_details <- v$circuit_details %>% mutate(site_id = as.character(site_id))
      v$circuit_details <- v$circuit_details %>% mutate(c_id = as.character(c_id))
      v$circuit_details <- filter(v$circuit_details, site_id %in% v$site_details$site_id)
      site_types <- c("pv_site_net", "pv_site", "pv_inverter_net", "pv_inverter")
      v$circuit_details <- filter(v$circuit_details, con_type %in% site_types)
      removeNotification(id)
    
      duration_options <- c("5", "30", "60")
      if (str_sub(time_series_file(), start=-7)=="feather"){
        # If a feather file is used it is assumed the data is pre-processed.
        # Hence the data is not passed to the raw data processor.
        id <- showNotification("Loading timeseries data from feather", duration=1000)
        ts_data <- read_feather(time_series_file())
        duration_sample_sizes <- get_duration_sample_counts(ts_data, duration_options)
        gc()
        ts_data <- filter(ts_data, d==duration())
        ts_data <- inner_join(ts_data, select(v$circuit_details, c_id), by="c_id")
        removeNotification(id)
      }else{
        id <- showNotification("Loading timeseries data from csv", duration=1000)
        cd_data <- v$circuit_details
        ts_data <- read.csv.sql(file = time_series_file(), 
                                sql="select * from file where c_id in (select c_id from cd_data)", eol = "\n")
        # Data from CSV is assumed to need processing.
        if ('utc_tstamp' %in% colnames(ts_data)) {ts_data <- setnames(ts_data, c("utc_tstamp"), c("ts"))}
        if ('t_stamp' %in% colnames(ts_data)) {ts_data <- setnames(ts_data, c("t_stamp"), c("ts"))}
        if ('voltage' %in% colnames(ts_data)) {ts_data <- setnames(ts_data, c("voltage"), c("v"))}
        if ('vrms' %in% colnames(ts_data)) {ts_data <- setnames(ts_data, c("vrms"), c("v"))}
        if ('voltage_max' %in% colnames(ts_data)) {ts_data <- setnames(ts_data, c("voltage_max"), c("v"))}
        if ('frequency' %in% colnames(ts_data)) {ts_data <- setnames(ts_data, c("frequency"), c("f"))}
        if ('energy' %in% colnames(ts_data)) {ts_data <- setnames(ts_data, c("energy"), c("e"))}
        if ('duration' %in% colnames(ts_data)) {ts_data <- setnames(ts_data, c("duration"), c("d"))}
        ts_data <- ts_data %>% distinct(c_id, ts, .keep_all=TRUE)
        ts_data <- mutate(ts_data, c_id = as.character(c_id))
        removeNotification(id)
        id <- showNotification("Formatting timeseries data and 
                                creating feather cache file", duration=1000)
        ts_data <- process_raw_time_series_data(ts_data)
        # Automatically create a cache of the processed data as a feather file.
        # Allows for much faster data loading for subsequent anaylsis.
        file_no_type = str_sub(time_series_file(), end=-5)
        file_type_feather = paste(file_no_type, '_', region_to_load(), ".feather", sep="")
        gc()
        write_feather(ts_data, file_type_feather)
        duration_sample_sizes <- get_duration_sample_counts(ts_data, duration_options)
        ts_data <- filter(ts_data, d==duration())
        removeNotification(id)
      }
      # Load in the install data from CSV.
      id <- showNotification("Load CER capacity data", duration=1000)
      intall_data_file <- "cumulative_capacity_and_number.csv"
      install_data <- read.csv(file=intall_data_file, header=TRUE, stringsAsFactors = FALSE)
      v$install_data <- process_install_data(install_data)
      
      # Load postcode lat and long data
      postcode_data_file <- "PostcodesLatLongQGIS.csv"
      postcode_data <- read.csv(file=postcode_data_file, header=TRUE, stringsAsFactors = FALSE)
      v$postcode_data <- process_postcode_data(postcode_data)
      removeNotification(id)
      
      # Combine the data needed to perform data cleaning.
      id <- showNotification("Combining data tables", duration=1000)
      v$circuit_details <- mutate(v$circuit_details, c_id=as.character(c_id))
      circuit_details_for_cleaning <- select(v$circuit_details, c_id, site_id,
                                             con_type, polarity)
      ts_data <- inner_join(ts_data, circuit_details_for_cleaning, by="c_id")
      site_details_for_cleaning <- select(v$site_details, site_id, s_postcode, first_ac)
      ts_data <- inner_join(ts_data, site_details_for_cleaning, by="site_id")
      ts_data <- perform_power_calculations(ts_data)

      removeNotification(id)
      
      if(frequency_data_file()!=''){
        v$frequency_data <- read.csv(file=frequency_data_file(), header=TRUE, stringsAsFactors = FALSE)
        v$frequency_data <- mutate(v$frequency_data, 
                                   ts=as.POSIXct(strptime(ts, "%Y-%m-%d %H:%M:%S", tz="Australia/Brisbane")))
      } else {
        v$frequency_data <- data.frame()
      }
      
      if (perform_clean()){
        id <- showNotification("Cleaning data", duration=1000)
        # Clean site details data
        site_details_cleaned <- site_details_data_cleaning(ts_data, v$site_details_raw)
        v$site_details_cleaned <- site_details_cleaned[order(site_details_cleaned$site_id),]
        # Clean circuit details file
        
        v$circuit_details_for_editing <- clean_connection_types(
          select(ts_data, ts, c_id, e, power_kW, s_postcode, con_type, polarity, first_ac), 
                 v$circuit_details, v$postcode_data)
        # Display data cleaning output in data cleaning tab
        output$site_details_editor <- renderDT(isolate(v$site_details_cleaned), selection='single', rownames=FALSE, 
                                               editable=TRUE)
        v$proxy_site_details_editor <- dataTableProxy('site_details_editor')
        output$circuit_details_editor <- renderDT(isolate(v$circuit_details_for_editing), selection='single', 
                                                  rownames=FALSE, editable=TRUE)
        v$proxy_circuit_details_editor <- dataTableProxy('circuit_details_editor')
        
        # Add cleaned data to display data for main tab
        output$save_cleaned_data <- renderUI({actionButton("save_cleaned_data", "Save cleaned data")})
        show("save_cleaned_data")
        removeNotification(id)
      } else {
        # Don't let the user crash the tool by trying to save data that doesn't exist
        hide("save_cleaned_data")
      }
      gc()
      v$combined_data <- select(ts_data, ts, c_id, e, v, f, d, power_kW)
      gc()
      remove(ts_data)
      gc()
      
      # Get offset filter options and label
      v$combined_data <- get_time_offsets(v$combined_data)
      v$unique_offsets <- get_time_series_unique_offsets(v$combined_data)
      sample_counts <- get_offset_sample_counts(v$combined_data, v$unique_offsets)
      unique_offsets_filter_label <- make_offset_filter_label(sample_counts, v$unique_offsets)
      #Update duration selection button
      base_label <- "Sampled duration (seconds), select one. Note durations are calculated based on most frequently 
                     occuring time between  measurements for each c_id. Sample sizes in last data set loaded 
                     (including all connection types) are "
      duration_label <- paste(base_label, "5:n=", duration_sample_sizes[1], ' ', 
                              "30:n=", duration_sample_sizes[2], ' ',
                              "60:n=", duration_sample_sizes[3], '.', sep='')
      output$duration <- renderUI({radioButtons("duration", label=strong(duration_label), 
                                                choices = list("5","30","60"), 
                                                selected = duration(), inline = TRUE)})
      
      # Filtering option widgets are rendered after the data is loaded, this is 
      # because they are only usable once there is data to filter. Additionally
      # The data loaded can then be used to create the appropraite options for 
      # filtering.
      if(perform_clean()){
        output$cleaned <- renderUI({
          checkboxGroupButtons(inputId="cleaned", label=strong("Data processing:"), choices=list("cleaned", "raw"),
                               selected=list("cleaned"), justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")))
        })
      } else {
        output$cleaned <- renderUI({
          checkboxGroupButtons(inputId="cleaned", label=strong("Data processing:"), choices=list("cleaned", "raw"),
                               selected=list("raw"), justified=TRUE, status="primary", individual=TRUE,
                               checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")))
        })
        
      }

      output$dateWidget <- renderUI({
        dateRangeInput("date", label=strong('Date range (yyyy-mm-dd):'),
                       start=strftime(floor_date(get_mode(v$combined_data$ts), "day"), format="%Y-%m-%d"),
                       end=strftime(floor_date(get_mode(v$combined_data$ts), "day"), format="%Y-%m-%d"),
                       min=strftime(floor_date(min(v$combined_data$ts), "day"), format="%Y-%m-%d"),
                       max=strftime(floor_date(max(v$combined_data$ts), "day"), format="%Y-%m-%d"), 
                       startview="year")
        })
      output$time_start <- renderUI({
        timeInput("time_start", label=strong('Enter start time'), value=as.POSIXct("18:00:00",format="%H:%M:%S"))
        })
      output$time_end <- renderUI({
        timeInput("time_end", label=strong('Enter end time'), value=as.POSIXct("19:00:00",format="%H:%M:%S"))
        })
      output$region <- renderUI({
        selectInput(inputId="region", label=strong("Region"), choices=unique(v$site_details$s_state))
        })
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
      shinyjs::show("Std_Agg_Indiv")
      output$size_groupings <- renderUI({
        checkboxGroupButtons(inputId="size_groupings", label=strong("Size Groupings"),
                             choices=list("30-100kW", "<30 kW"), selected=list("30-100kW", "<30 kW"),
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"),no=icon("remove", lib="glyphicon")))
      })
      output$StdVersion <- renderUI({
        checkboxGroupButtons(inputId="StdVersion", 
                             label=strong("AS47777 Version:"), choices=list("AS4777.3:2005", "Transition", 
                                                                            "AS4777.2:2015"),
                             selected=list("AS4777.3:2005", "Transition", "AS4777.2:2015"),
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")))
        })
      output$responses <- renderUI({
        checkboxGroupButtons(inputId="responses", 
                             label=strong("Select Responses:"),
                             choices=list("1 Ride Through", "2 Curtail", "3 Drop to Zero", "4 Disconnect","5 Off at t0", 
                                          "6 Not enough data", "Undefined", "NA"),
                             selected=list("1 Ride Through", "2 Curtail", "3 Drop to Zero", "4 Disconnect",
                                           "5 Off at t0", "6 Not enough data", "Undefined", "NA"),
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")))
      })
      output$zones <- renderUI({
        checkboxGroupButtons(inputId="zones", label=strong("Zones"), 
                             choices=list("1 Zone", "2 Zone", "3 Zone", "Undefined", "NA"),
                             selected=list("1 Zone", "2 Zone", "3 Zone", "Undefined", "NA"), 
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")))
      })
      output$compliance <- renderUI({
        checkboxGroupButtons(inputId="compliance", label=strong("Compliance"), 
                             choices=list("Compliant", "Non-compliant Responding", 
                                          "Non-compliant", "Disconnect/Drop to Zero",
                                          "Off at t0", "Not enough data", "Undefined", "NA"),
                             selected=list("Compliant", "Non-compliant Responding", 
                                           "Non-compliant", "Disconnect/Drop to Zero",
                                           "Off at t0", "Not enough data", "Undefined", "NA"), 
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")))
      })  
      output$offsets <- renderUI({
        checkboxGroupButtons(inputId="offsets", label=unique_offsets_filter_label, 
                             choices=v$unique_offsets, selected=c(v$unique_offsets[which.max(sample_counts)]) ,
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"), no=icon("remove", lib="glyphicon")))
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
      output$event_date <- renderUI({
        dateInput("event_date", label=strong('Event date (yyyy-mm-dd):'), 
                  value=strftime(floor_date(get_mode(v$combined_data$ts), "day"), format="%Y-%m-%d"), startview="year")
      })
      output$pre_event_interval <- renderUI({
        timeInput("pre_event_interval", label=strong('Pre-event time interval (Needs to match exactly to data timestamp)'), 
                  value = as.POSIXct("18:05:55",format="%H:%M:%S"))
      })
      # output$event_time <- renderUI({
      #   timeInput("event_time", label=strong('Time of maximum pv response (Needs to match exactly to data timestamp)'), 
      #             value = as.POSIXct("18:11:55",format="%H:%M:%S"))
      # })
      output$window_length <- renderUI({
        numericInput("window_length", label=strong('Set window length (min),
                                                   Only data in this window is used for response analysis.'), value=5, min = 1, max = 100, step = 1)
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
    #}, warning = function(war) {
    #  shinyalert("Opps", paste("",war))
    #  reset_sidebar(input, output, session)
    #  reset_chart_area(input, output, session)
    #  reset_data_cleaning_tab(input, output, session)
    #}, error = function(err) {
    #  shinyalert("Opps", paste("",err))
    #  reset_sidebar(input, output, session)
    #  reset_chart_area(input, output, session)
    #  reset_data_cleaning_tab(input, output, session)
    #}, finally = {
    #  reset_chart_area(input, output, session)
    #  removeNotification(id)
    #})
  })

  # Create plots when update plots button is clicked.
  observeEvent(input$update_plots, {
    id <- showNotification("Updating plots", duration=1000)
    
    # Get ideal response.
    if(dim(v$frequency_data)[1]>0){
      temp_f_data <- select(v$frequency_data, ts, region()) 
      temp_f_data <- setnames(temp_f_data, c(region()), c("f"))
      temp_f_data <- mutate(temp_f_data, f=as.numeric(f))
      ideal_response_to_plot <- ideal_response(temp_f_data)
    } else {
      ideal_response_to_plot <- data.frame()
    }
    v$ideal_response_to_plot <- ideal_response_to_plot
    
    
    # Filter to just the time window being plotted/analyised. 
    combined_data_f <- filter(v$combined_data, ts>=start_time() & ts<= end_time())
    
    # Data frame for after adding meta data.
    combined_data_f2 <- data.frame()
    
    # If the raw data is needed for analysis then combine time series data and
    # raw meta data.
    if ('raw' %in% clean()){
      combined_data_raw <- combine_data_tables(
        select(combined_data_f, ts, c_id, e, v, f, d, time_offset),  
        select(v$circuit_details, c_id, site_id, con_type, polarity),
        v$site_details)
      combined_data_raw <- combined_data_raw %>% mutate(clean="raw")
      combined_data_raw <- select(combined_data_raw, c_id, ts, v, f, d, site_id,
                                  e, con_type, s_state, s_postcode, 
                                  Standard_Version, Grouping, polarity, first_ac,
                                  power_kW, clean, manufacturer, model, sum_ac, 
                                  time_offset)
      combined_data_f2 <- bind_rows(combined_data_f2, combined_data_raw)
      remove(combined_data_raw)
    }
    
    
    # If the cleaned data is needed for analysis then combine time series data
    # and cleaned meta data.
    if (perform_clean() & 'cleaned' %in% clean()){
    site_details_cleaned_processed <- process_raw_site_details(v$site_details_cleaned)
      combined_data_clean <- combine_data_tables(
        select(combined_data_f, ts, c_id, e, v, f, d, time_offset),  
        select(v$circuit_details_for_editing, c_id, site_id, con_type, polarity),
        site_details_cleaned_processed)
      combined_data_clean <- combined_data_clean %>% mutate(clean="cleaned")
      combined_data_clean <- select(combined_data_clean, c_id, ts, v, f, d, 
                                    site_id, e, con_type, s_state, s_postcode, 
                                    Standard_Version, Grouping, polarity, 
                                    first_ac, power_kW, clean, manufacturer, model, 
                                    sum_ac, time_offset)
      combined_data_f2 <- bind_rows(combined_data_f2, combined_data_clean)
      remove(combined_data_clean)
    }
    
    combined_data_f <- combined_data_f2
    gc()
  
    
    # Set default manual cleaning value.
    combined_data_f <- mutate(combined_data_f, manual_compliance = 'Not set')

    # Perform meta data filtering.    
    combined_data_f <- filter(combined_data_f, sum_ac<=100)
    gc()
    combined_data_f <- filter(combined_data_f, s_state==region())
    if (length(clean()) < 2) {combined_data_f <- filter(combined_data_f, clean %in% clean())}
    if (length(size_groupings()) < 2) {combined_data_f <- filter(combined_data_f, Grouping %in% size_groupings())}
    if (length(standards()) < 3) {combined_data_f <- filter(combined_data_f, Standard_Version %in% standards())}
    if (length(postcodes()) > 0) {combined_data_f <- filter(combined_data_f, s_postcode %in% postcodes())}
    if (length(manufacturers()) > 0) {combined_data_f <- filter(combined_data_f, manufacturer %in% manufacturers())}
    if (length(models()) > 0) {combined_data_f <- filter(combined_data_f, model %in% models())}
    if (length(sites()) > 0) {combined_data_f <- filter(combined_data_f, site_id %in% sites())}
    if (length(circuits()) > 0) {combined_data_f <- filter(combined_data_f, c_id %in% circuits())}
    if(length(combined_data_f$ts) > 0){
      combined_data_f <- categorise_response(combined_data_f, pre_event_interval(), window_length())
      if (length(responses()) < 6) {combined_data_f <- filter(combined_data_f, response_category %in% responses())}
    }
    # Filter data by user selected time window
    if(length(combined_data_f$ts) > 0){
      combined_data_f <- get_distance_from_event(combined_data_f, v$postcode_data, event_latitude(), event_longitude())
      combined_data_f <- get_zones(combined_data_f, zone_one_radius(), zone_two_radius(), zone_three_radius())
      if (length(zones()) < 3) { combined_data_f <- filter(combined_data_f, zone %in% zones())}
    }
    
    if (length(offsets()) < length(v$unique_offsets)) { 
      combined_data_f <- filter(combined_data_f, time_offset %in% offsets())
    }
  
    # Decide if the settings mean no grouping is being performed.
    if (agg_on_standard()==FALSE & pst_agg()==FALSE & grouping_agg()==FALSE & manufacturer_agg()==FALSE & 
        model_agg()==FALSE & zone_agg()==FALSE & circuit_agg()==TRUE & compliance_agg()==TRUE){
      no_grouping=TRUE
    } else {
      no_grouping=FALSE
    }
    
    # Calc event normalised power
    event_powers <- filter(combined_data_f, ts==pre_event_interval())
    event_powers <- mutate(event_powers, event_power=power_kW)
    event_powers <- select(event_powers, c_id, clean, event_power)
    combined_data_f <- left_join(combined_data_f, event_powers, by=c("c_id", "clean"))
    combined_data_f <- mutate(combined_data_f, c_id_norm_power=power_kW/event_power)
    
    if(length(combined_data_f$ts) > 0){
      # Calaculate the site peformance factors.
      id2 <- showNotification("Calculating site performance factors", duration=1000)
      combined_data_f <- calc_site_performance_factors(combined_data_f)
      combined_data_f <- setnames(combined_data_f, c("ts"), c("Time"))
      combined_data_f <- event_normalised_power(combined_data_f, pre_event_interval(), keep_site_id=TRUE)
      combined_data_f <- setnames(combined_data_f, c("Event_Normalised_Power_kW"), c("Site_Event_Normalised_Power_kW"))
      combined_data_f <- setnames(combined_data_f, c("Time"), c("ts"))
      if(dim(ideal_response_to_plot)[1]>0){
        ideal_response_downsampled <- down_sample_1s(ideal_response_to_plot, duration(), min(combined_data_f$ts))
        v$ideal_response_downsampled <- ideal_response_downsampled
        combined_data_f <- 
          calc_error_metric_and_compliance_2(combined_data_f, 
                                             ideal_response_downsampled,
                                             ideal_response_to_plot,
                                             compliance_threshold(),
                                             start_buffer(),
                                             end_buffer(),
                                             end_buffer_responding(),
                                             disconnecting_threshold())
      } else {
        combined_data_f <- mutate(combined_data_f, compliance_status="Undefined")  
      }
      if (length(compliance()) < 8) {combined_data_f <- filter(combined_data_f, compliance_status %in% compliance())}
      removeNotification(id2)
    }
    
    
    # Count samples in each data series to be displayed
    grouping_cols <- find_grouping_cols(agg_on_standard=agg_on_standard(), pst_agg=pst_agg(), 
                                        grouping_agg=grouping_agg(), manufacturer_agg=manufacturer_agg(), 
                                        model_agg=model_agg(), circuit_agg(), response_agg(), zone_agg(),
                                        compliance_agg())
    if(length(combined_data_f$ts) > 0){
      v$sample_count_table <- vector_groupby_count(combined_data_f, grouping_cols)
    }
    
    # Procced to  aggregation and plotting only if there is less than 1000 data series to plot, else stop and notify the
    # user.
    if ((sum(v$sample_count_table$sample_count)<1000 & no_grouping) | 
        (length(v$sample_count_table$sample_count)<1000 & !no_grouping)){
      if(length(combined_data_f$ts) > 0){
      # Copy data for saving
      v$combined_data_f <- select(combined_data_f, ts, site_id, c_id, power_kW, c_id_norm_power, v, f, s_state, s_postcode, 
                                  Standard_Version, Grouping, sum_ac, clean, manufacturer, model,
                                  site_performance_factor, response_category, zone, distance, lat, lon, e, con_type,
                                  first_ac, polarity, compliance_status, manual_compliance)
      # Create copy of filtered data to use in upscaling
      combined_data_f2 <- combined_data_f
        if(raw_upscale()){combined_data_f2 <- upscale(combined_data_f2, v$install_data)}
      }
      # Check that the filter does not result in an empty dataframe.
      if(length(combined_data_f$ts) > 0){
        agg_norm_power <- vector_groupby_norm_power(combined_data_f, grouping_cols)
        agg_f_and_v <- vector_groupby_f_and_v(combined_data_f, grouping_cols)
        v$agg_power <- vector_groupby_power(combined_data_f2, grouping_cols)
        v$response_count <- vector_groupby_count_response(combined_data_f, grouping_cols)
        v$zone_count <- vector_groupby_count_zones(combined_data_f, grouping_cols)
        v$distance_response <- vector_groupby_cumulative_distance(combined_data_f, grouping_cols)
        geo_data <- vector_groupby_system(combined_data_f, grouping_cols)
        v$circuit_summary <- distinct(combined_data_f, c_id, .keep_all=TRUE)
        v$circuit_summary <- select(v$circuit_summary, site_id, c_id, s_state, s_postcode, Standard_Version, Grouping, 
                                    sum_ac, clean, manufacturer, model, response_category, zone, distance, lat, lon,
                                    con_type, first_ac, polarity, compliance_status, manual_compliance)
        
        # Combine data sets that have the same grouping so they can be saved in a single file
        if (no_grouping){
          et <- pre_event_interval()
          # agg_norm_power <- event_normalised_power(agg_norm_power, et, keep_site_id=TRUE)
          v$agg_power <- left_join( v$agg_power, agg_norm_power[, c("c_id_norm_power", "c_id", "Time")], 
                                    by=c("Time", "c_id"))
        } else {
          et <- pre_event_interval()
          # agg_norm_power <- event_normalised_power(agg_norm_power, et,  keep_site_id=FALSE)
          v$agg_power <- left_join(v$agg_power, agg_norm_power[, c("c_id_norm_power", "series", "Time")], 
                                   by=c("Time", "series"))
        }
        v$agg_power <- left_join( v$agg_power, agg_f_and_v[, c("Time", "series", "Voltage", "Frequency")], 
                                  by=c("Time", "series"))
        
        
        # Create plots on main tab
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
          output$save_report <- renderUI({
            shinyDirButton("save_report", "Save report", "Choose directory for report files ...")
          })
          output$save_ideal_response <- renderUI({
            shinySaveButton("save_ideal_response", "Save response", "Choose directory for report files ...")
          })
          output$save_ideal_response_downsampled <- renderUI({
            shinySaveButton("save_ideal_response_downsampled", "Save downsampled response", "Choose directory for report files ...")
          })
          output$sample_count_table <- renderDataTable({v$sample_count_table})
          output$save_sample_count <- renderUI({shinySaveButton("save_sample_count", "Save data", "Save file as ...", 
                                                                filetype=list(xlsx="csv"))
            })
          if(dim(ideal_response_to_plot)[1]>0){
            output$NormPower <- renderPlotly({
              plot_ly(agg_norm_power, x=~Time, y=~c_id_norm_power, color=~series, type="scattergl") %>% 
                add_trace(x=~ideal_response_to_plot$ts, y=~ideal_response_to_plot$norm_power, name='Ideal Response', 
                          mode='markers', inherit=FALSE) %>%
                add_trace(x=~ideal_response_downsampled$time_group, y=~ideal_response_downsampled$norm_power, 
                          name='Ideal Response Downsampled', mode='markers', inherit=FALSE) %>%
                layout(yaxis=list(title="Circuit power normalised to value of pre-event interval, \n aggregated by averaging"))
            })
          } else {
            output$NormPower <- renderPlotly({
              plot_ly(agg_norm_power, x=~Time, y=~c_id_norm_power, color=~series, type="scattergl", 
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
                add_trace(x=~temp_f_data$ts, y=~temp_f_data$f, name='High Speed Data', 
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
          z1<- data.frame(circle.polygon(event_longitude(), event_latitude(), zone_one_radius(), sides = 20, units='km', poly.type = "gc.earth"))
          z2 <- data.frame(circle.polygon(event_longitude(), event_latitude(), zone_two_radius(), sides = 20, units='km', poly.type = "gc.earth"))
          z3 <- data.frame(circle.polygon(event_longitude(), event_latitude(), zone_three_radius(), sides = 20, units='km', poly.type = "gc.earth"))
          output$map <- renderPlotly({plot_geo(geo_data, lat=~lat, lon=~lon, color=~percentage_disconnect) %>%
              add_polygons(x=~z1$lon, y=~z1$lat, inherit=FALSE, fillcolor='transparent', 
                           line=list(width=2,color="grey"), hoverinfo = "none", showlegend=FALSE) %>%
              add_polygons(x=~z2$lon, y=~z2$lat, inherit=FALSE, fillcolor='transparent', 
                           line=list(width=2,color="grey"), hoverinfo = "none", showlegend=FALSE) %>%
              add_polygons(x=~z3$lon, y=~z3$lat, inherit=FALSE, fillcolor='transparent', 
                           line=list(width=2,color="grey"), hoverinfo = "none", showlegend=FALSE) %>%
              add_markers(x=~geo_data$lon, y=~geo_data$lat,  inherit=FALSE, 
                          hovertext=~geo_data$info, legendgroup = list(title = "Percentage Disconnects"),
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
                                   choices = list("cleaned","raw"), 
                                   selected = v$combined_data_f$clean[1], 
                                   inline = TRUE)})
          
          removeNotification(id)
          
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
  })
  
  observeEvent(input$compliance_cleaned_or_raw, {
    if(compliance_cleaned_or_raw() %in% v$combined_data_f$clean){
      # Setting up manual compliance tab.
      circuit_options <- filter(v$combined_data_f, clean==compliance_cleaned_or_raw())
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
                                                                 choices = list("cleaned","raw"), 
                                                                 selected = v$combined_data_f$clean[1], 
                                                                 inline = TRUE)})
    }
  })
  
  # Change site displayed in manual compliance tab.
  observeEvent(input$compliance_circuits, {
    if (compliance_circuits() %in% v$c_id_vector){
      v$compliance_counter <- match(c(compliance_circuits()), v$c_id_vector)
      message <- paste0("Select circuit (now viewing circuit ", v$compliance_counter, ' of ', length(v$c_id_vector) ,")")
      circuit_to_view <- v$c_id_vector[[v$compliance_counter]]
      output$compliance_circuits <- isolate(renderUI({selectizeInput("compliance_circuits", label=strong(message), choices=as.list(v$c_id_vector), 
                                                          multiple=FALSE, selected=circuit_to_view)}))
      data_to_view <- filter(filter(v$combined_data_f, clean==compliance_cleaned_or_raw()), c_id==compliance_circuits())
      output$set_c_id_compliance <- renderUI({radioButtons("set_c_id_compliance", 
                                                                   label=strong("Compliance"), 
                                                                   choices = list("Not set","Compliant","Non-compliant", 
                                                                                  "Non-compliant Responding", "Disconnect", 
                                                                                  "Unsure"), 
                                                                   selected = data_to_view$manual_compliance[1], inline = TRUE)})
      updateRadioButtons(session, "set_c_id_compliance", 
                         selected = data_to_view$manual_compliance[1])
      data_to_view <- mutate(data_to_view, Time=ts)
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
      shinyalert("Wow", "That site id does not exist.")
    }
  })
  
  observeEvent(input$set_c_id_compliance, {
    current_c_id <- v$c_id_vector[[v$compliance_counter]]
    v$combined_data <- mutate(v$combined_data, 
                              manual_compliance=
                                ifelse((c_id==current_c_id) & 
                                         (clean==compliance_cleaned_or_raw()),
                                       set_c_id_compliance(),
                                       manual_compliance))
    v$combined_data_f <- mutate(v$combined_data_f, 
                                manual_compliance=
                                  ifelse((c_id==current_c_id) & 
                                           (clean==compliance_cleaned_or_raw()),
                                         set_c_id_compliance(),
                                         manual_compliance))
    
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
  
  # observeEvent(input$save_report, {
  #   volumes <- c(home=getwd())
  #   shinyDirChoose(input, "save_report", roots=volumes, session=session)
  #   datapath <- parseDirPath(volumes, input$save_report)
  #   if(length(datapath) > 0){
  #     id <- showNotification("Creating report", duration=1000)
  #     create_files(v$agg_power, v$combined_data_f, pre_event_interval(), 
  #                  event_time(), datapath, zone_one_radius(), zone_two_radius(), 
  #                  zone_three_radius())
  #     variables <- c('time_series_file', 'circuit_details_file', 'site_details_file', 'frequency_data_file', 'region',
  #                    'duration', 'standards', 'responses', 'postcodes', 'manufacturers', 'models', 'sites', 'circuits', 
  #                    'zones', 'compliance', 'offsets', 'size_groupings', 'clean', 'raw_upscale', 'pst_agg', 
  #                    'grouping_agg', 'response_agg', 'manufacturer_agg', 'perform_clean', 'model_agg', 'circuit_agg', 
  #                    'zone_agg', 'compliance_agg', 'start_time', 'end_time', 'pre_event_interval', 
  #                    'agg_on_standard', 'window_length', 'event_latitude', 'event_longitude', 'zone_one_radius', 
  #                    'zone_two_radius', 'zone_three_radius')
  #     values <- c(time_series_file(), circuit_details_file(), site_details_file(), frequency_data_file(), 
  #                 if(is.null(region())){''}else{region()},
  #                 if(is.null(duration())){''}else{duration()}, paste(standards(), collapse='; '), paste(responses(), collapse='; '), 
  #                 paste(postcodes(), collapse='; '), 
  #                 paste(manufacturers(), '; '), 
  #                 paste(models(), collapse='; '), paste(sites(), collapse='; '), paste(circuits(), collapse='; '), 
  #                 paste(zones(), collapse='; '),  paste(compliance(), collapse='; '), paste(offsets(), collapse='; '), 
  #                 paste(size_groupings(), collapse='; '),
  #                 paste(clean(), collapse='; '), raw_upscale(), pst_agg(), 
  #                 grouping_agg(), response_agg(), manufacturer_agg(), perform_clean(), model_agg(), circuit_agg(), 
  #                 zone_agg(), compliance_agg(), 
  #                 if(length(start_time())==0){''}else{as.character(start_time())}, 
  #                 if(length(end_time())==0){''}else{as.character(end_time())}, 
  #                 if(length(pre_event_interval())==0){''}else{as.character(pre_event_interval())},
  #                 agg_on_standard(), 
  #                 if(is.null(window_length())){''}else{window_length()}, 
  #                 if(is.null(event_latitude())){''}else{event_latitude()}, 
  #                 if(is.null(event_longitude())){''}else{event_longitude()}, 
  #                 if(is.null(zone_one_radius())){''}else{zone_one_radius()}, 
  #                 if(is.null(zone_two_radius())){''}else{zone_two_radius()}, 
  #                 if(is.null(zone_three_radius())){''}else{zone_three_radius()})
  #     meta_data = data.frame(variables, values, stringsAsFactors = FALSE)
  #     write.csv(meta_data, paste0(datapath, '_meta_data.csv'), row.names=FALSE)
  #     write.csv(v$circuit_summary, paste0(datapath, '_circ_sum.csv'), row.names=FALSE)
  #     removeNotification(id)
  #   }
  # 
  # })
  
  # Save data from aggregate pv power plot
  observeEvent(input$batch_save, {
    variables <- c('time_series_file', 'circuit_details_file', 'site_details_file', 'frequency_data_file', 'region',
                   'duration', 'standards', 'responses', 'postcodes', 'manufacturers', 'models', 'sites', 'circuits', 
                   'zones', 'compliance', 'offsets', 'size_groupings', 'clean', 'raw_upscale', 'pst_agg', 
                   'grouping_agg', 'response_agg', 'manufacturer_agg', 'perform_clean', 'model_agg', 'circuit_agg', 
                   'zone_agg', 'compliance_agg', 'start_time', 'end_time', 'pre_event_interval', 
                   'agg_on_standard', 'window_length', 'event_latitude', 'event_longitude', 'zone_one_radius', 
                   'zone_two_radius', 'zone_three_radius', 'compliance_threshold', 'start_buffer', 'end_buffer',
                   'end_buffer_responding', 'disconnecting_threshold')
   values <- c(time_series_file(), circuit_details_file(), site_details_file(), frequency_data_file(), 
                   if(is.null(region())){''}else{region()},
                   if(is.null(duration())){''}else{duration()}, paste(standards(), collapse='; '), paste(responses(), collapse='; '), 
                   paste(postcodes(), collapse='; '), 
                   paste(manufacturers(), '; '), 
                   paste(models(), collapse='; '), paste(sites(), collapse='; '), paste(circuits(), collapse='; '), 
                   paste(zones(), collapse='; '),  paste(compliance(), collapse='; '), paste(offsets(), collapse='; '), 
                   paste(size_groupings(), collapse='; '),
                   paste(clean(), collapse='; '), raw_upscale(), pst_agg(), 
                   grouping_agg(), response_agg(), manufacturer_agg(), perform_clean(), model_agg(), circuit_agg(), 
                   zone_agg(), compliance_agg(), 
                   if(length(start_time())==0){''}else{as.character(start_time())}, 
                   if(length(end_time())==0){''}else{as.character(end_time())}, 
                   if(length(pre_event_interval())==0){''}else{as.character(pre_event_interval())},
                   agg_on_standard(), 
                   if(is.null(window_length())){''}else{window_length()}, 
                   if(is.null(event_latitude())){''}else{event_latitude()}, 
                   if(is.null(event_longitude())){''}else{event_longitude()}, 
                   if(is.null(zone_one_radius())){''}else{zone_one_radius()}, 
                   if(is.null(zone_two_radius())){''}else{zone_two_radius()}, 
                   if(is.null(zone_three_radius())){''}else{zone_three_radius()},
                   compliance_threshold(), start_buffer(), end_buffer(),
                   end_buffer_responding(), disconnecting_threshold())
   
    meta_data = data.frame(variables, values, stringsAsFactors = FALSE)
    volumes <- getVolumes()
    shinyFileSave(input, "batch_save", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$batch_save)
    if (nrow(fileinfo) > 0) {
      id <- showNotification("Doing batch save", duration=1000)
      datapath <- strsplit(fileinfo$datapath, '[.]')[[1]][1]
      write.csv(v$agg_power, paste0(datapath, '_agg_power.csv'), row.names=FALSE)
      write.csv(v$combined_data_f, paste0(datapath, '_underlying.csv'), row.names=FALSE)
      write.csv(v$circuit_summary, paste0(datapath, '_circ_sum.csv'), row.names=FALSE)
      write.csv(v$sample_count_table, paste0(datapath, '_sample.csv'), row.names=FALSE)
      write.csv(v$response_count, paste0(datapath, '_response_count.csv'), row.names=FALSE)
      write.csv(v$zone_count, paste0(datapath, '_zone_count.csv'), row.names=FALSE)
      write.csv(v$distance_response, paste0(datapath, '_distance_response.csv'), row.names=FALSE)
      write.csv(meta_data, paste0(datapath, '_meta_data.csv'), row.names=FALSE)
      removeNotification(id)
    }
  })
  
  
  # Time series file selection pop up.
  observe({
    volumes <- c(home=getwd())
    shinyFileChoose(input, "choose_ts", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_ts)
    if (nrow(fileinfo) > 0) {updateTextInput(session, "time_series", value=as.character(fileinfo$datapath))}
  })
  
  # Circuit details file selection pop up.
  observe({
    volumes <- c(home=getwd())
    shinyFileChoose(input, "choose_c", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_c)
    if (nrow(fileinfo) > 0) {updateTextInput(session, "circuit_details", value=as.character(fileinfo$datapath))}
  })
  
  # Site details file selection pop up.
  observe({
    volumes <- c(home=getwd())
    shinyFileChoose(input, "choose_site", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_site)
    if (nrow(fileinfo) > 0) {updateTextInput(session, "site_details", value=as.character(fileinfo$datapath))}
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
       | compliance_agg() | grouping_agg()){
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
      output$site_plot <- renderPlotly({plot_ly(data_to_view, x=~ts, y=~power_kW, type="scatter")})
    }
    
  })
  
  # Plot the data for the site selected in the table
  observeEvent(input$site_details_editor_rows_selected, {
    v$proxy_circuit_details_editor %>% selectRows(NULL)
    if (length(input$site_details_editor_rows_selected==1)) {
      site_id_to_plot <- v$site_details_cleaned$site_id[input$site_details_editor_rows_selected]
      circuits <- filter(v$circuit_details_for_editing, site_id==site_id_to_plot)
      circuits <- unique(circuits$c_id)
      data_to_view <- filter(v$combined_data, c_id %in% circuits)
      output$site_plot <- renderPlotly({plot_ly(data_to_view, x=~ts, y=~power_kW, color=~c_id, type="scatter")})
    }
  })
  
  # Save data cleaning tables to csv, also override data currently used as "cleaned" in tool.
  observeEvent(input$save_cleaned_data, {
    # Save cleaned data to csv
    id <- showNotification("Saving cleaned files", duration=1000)
    file_no_type = str_sub(site_details_file(), end=-5)
    new_file_name = paste(file_no_type, "_cleaned.csv", sep="")
    v$site_details_cleaned <- v$site_details_cleaned %>% mutate(site_id=as.character(site_id))
    write.csv(v$site_details_cleaned, new_file_name)
    file_no_type = str_sub(circuit_details_file(), end=-5)
    new_file_name = paste(file_no_type, "_cleaned.csv", sep="")
    write.csv(v$circuit_details_for_editing, new_file_name)
    removeNotification(id)
  })
  
  # Allow user to edit site details in data cleaning tab
  observeEvent(input$site_details_editor_cell_edit, {
    info = input$site_details_editor_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    value = info$value
    v$site_details_cleaned[i, j] <<- DT::coerceValue(value, v$site_details_cleaned[i, j])
    replaceData(v$proxy_site_details_editor, v$site_details_cleaned, resetPaging=FALSE, rownames=FALSE)  # important
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
  })
}


shinyApp(ui = ui, server = server)

