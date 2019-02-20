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
source("data_manipulation_functions.R")
source("filter_and_aggregate.R")
source("upscale_function.R")
source("data_cleaning_functions.R")
source("normalised_power_function.R")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  # Allows for the use of notifications.
  useShinyjs(),
  titlePanel("PV System Disturbance Analysis"),
  # Input Bar
  tabsetPanel(
    tabPanel("Main",fluid=TRUE,
      sidebarLayout(
        sidebarPanel(
          h4("File selection"),
          textInput("time_series", "Time series file", 
                    value="C:/Users/N.gorman/Documents/GitHub/DER_disturbance_analysis/2018-08-25 raw inputs/2018-08-25_sa_qld_naomi.feather"
          ),
          shinyFilesButton("choose_ts", "Choose File", 
                      "Select timeseries data file ...", multiple=FALSE
          ),
          HTML("<br><br>"),
          textInput("circuit_details", "Circuit details file", 
                    value="C:/Users/N.gorman/Documents/GitHub/DER_disturbance_analysis/2018-08-25 raw inputs/circuit_details.csv"
          ),

          shinyFilesButton("choose_c", "Choose File", 
                           "Select circuit details data file ...", multiple=FALSE
          ),
          HTML("<br><br>"),
          textInput("site_details", "Site details file", 
                    value="C:/Users/N.gorman/Documents/GitHub/DER_disturbance_analysis/2018-08-25 raw inputs/site_details.csv"
          ),
          shinyFilesButton("choose_site", "Choose File", 
                           "Select site details data file ...", multiple=FALSE
          ),
          HTML("<br><br>"),
          radioButtons("duration", 
                       label=strong("Sampled duration (seconds), select one"),
                       choices = list("5","30","60"), selected = "60", 
                       inline = TRUE),
          actionButton("load_data", "Load data"),
          tags$hr(),
          h4("Time Filter"),
          uiOutput("dateWidget"),
          uiOutput("time_start"),
          uiOutput("time_end"),
          tags$hr(),
          h4("Catergory Filter"),
          uiOutput("cleaned"),
          uiOutput("region"),
          #uiOutput("duration"),
          uiOutput("StdVersion"),
          uiOutput("size_groupings"),
          uiOutput("postcodes"),
          uiOutput("manufacturers"),
          uiOutput("models"),
          tags$hr(),
          h4("Aggregation Settings"),
          materialSwitch("Std_Agg_Indiv", label=strong("AS4777 Aggregated:"), 
                         status="primary"),
          materialSwitch("grouping_agg", 
                         label=strong("Size Grouping Aggregated:"), 
                         status="primary"),
          materialSwitch("pst_agg", label=strong("Postcodes Aggreagted:"), 
                         status="primary", value=TRUE),
          materialSwitch("manufacturer_agg", 
                         label=strong("Manufacturer Aggreagted:"), 
                         status="primary", value=TRUE),
          materialSwitch("model_agg", label=strong("Models Aggreagted:"), 
                         status="primary", value=TRUE),
          materialSwitch(inputId="raw_upscale", label=strong("Upscaled Data"), 
                         status="primary", right=FALSE),
          tags$hr(),
          h4("Event time"),
          uiOutput("event_date"),
          uiOutput("event_time"),
          tags$hr(),
          uiOutput("update_plots")
        ),
        #Output
        mainPanel(
          plotlyOutput(outputId="PlotlyTest"),
          uiOutput("save_agg_power"),
          HTML("<br>"),
          uiOutput("save_underlying"),
          HTML("<br><br>"),
          dataTableOutput("sample_count_table"),
          HTML("<br><br>"),
          uiOutput("save_sample_count"),
          plotlyOutput(outputId="NormPower"),
          plotlyOutput(outputId="Frequency"),
          plotlyOutput(outputId="Voltage")
        )
      )
    ),
    tabPanel("Data Cleaning", fluid=TRUE, 
      mainPanel(
        plotlyOutput("site_plot"),
        DTOutput('site_details_editor'),
        DTOutput('circuit_details_editor'),
        actionButton("save_cleaned_data", "Save cleaned data")
      )
    )
  ),
  useShinyalert()
)

server <- function(input,output,session){
  # Hide these inputs by default, they are shown once data is loaded.
  hide("Std_Agg_Indiv")
  hide("raw_upscale")
  hide("pst_agg")
  hide("grouping_agg")
  hide("manufacturer_agg")
  hide("model_agg")
  options(DT.options = list(pageLength = 3))
  # Get input from GUI
  time_series_file <- reactive({input$time_series})
  circuit_details_file <- reactive({input$circuit_details})
  site_details_file <- reactive({input$site_details})
  region <- reactive({input$region})
  duration <- reactive({input$duration})
  standards <- reactive({input$StdVersion})
  postcodes <- reactive({input$postcodes})
  manufacturers <- reactive({input$manufacturers})
  models <- reactive({input$models})
  size_groupings <- reactive({input$size_groupings})
  clean <- reactive({input$cleaned})
  raw_upscale <- reactive({input$raw_upscale})
  pst_agg <- reactive({input$pst_agg})
  grouping_agg <- reactive({input$grouping_agg})
  manufacturer_agg <- reactive({input$manufacturer_agg})
  model_agg <- reactive({input$model_agg})
  start_time <- reactive({
    date_as_str <- as.character(input$date[1])
    time_as_str <- substr(input$time_start, 12,19)
    start_time_as_str <- paste(date_as_str, time_as_str)
    start_date_time <- strftime(start_time_as_str)
    start_date_time
  })
  end_time <- reactive({
    date_as_str <- as.character(input$date[2])
    time_as_str <- substr(input$time_end, 12,19)
    end_time_as_str <- paste(date_as_str, time_as_str)
    end_date_time <- strftime(end_time_as_str)
    end_date_time
  })
  event_time <- reactive({
    date_as_str <- as.character(input$event_date)
    time_as_str <- substr(input$event_time, 12, 19)
    date_time_as_str <- paste(date_as_str, time_as_str)
    event_date_time <- strftime(date_time_as_str)
    event_date_time
  })
  agg_on_standard <- reactive({input$Std_Agg_Indiv})
  
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
                      combined_data_f = data.frame()
                      )
  
  # This is the event that runs when the "Load data" button on the GUI is
  # Clicked. 
  observeEvent(input$load_data, {
    # Users may enter invalid file paths or attempt to load data that creates
    # an error or warning during the data table combing process. The "tryCatch"
    # function catches these, aborts the loading process and reports the error 
    # to the users. Importantly this prevent the app from crashing.
    result = tryCatch({
      # Load data from storage.
      if (str_sub(time_series_file(), start=-7)=="feather"){
        # If a feather file is used it is assumed the data is pre-processed.
        # Hence the data is not passed to the raw data processor.
        id <- showNotification("Loading timeseries data from feather", duration=1000)
        time_series_data <- read_feather(time_series_file())
        time_series_data <- filter(time_series_data, d==duration())
        removeNotification(id)
      }else{
        id <- showNotification("Loading timeseries data from csv", duration=1000)
        time_series_data <- read.csv(file=time_series_file(), header=TRUE, 
                                     stringsAsFactors = FALSE)
        removeNotification(id)
        id <- showNotification("Formatting timeseries data and 
                                creating feather cache file", duration=1000)
        # Data from CSV is assumed to need processing.
        time_series_data <- time_series_data %>% distinct(c_id, ts, .keep_all=TRUE)
        time_series_data <- process_raw_time_series_data(time_series_data)
        # Automatically create a cache of the processed data as a feather file.
        # Allows for much faster data loading for subsequent anaylsis.
        file_no_type = str_sub(time_series_file(), end=-4)
        file_type_feather = paste(file_no_type, "feather", sep="")
        write_feather(time_series_data, file_type_feather)
        time_series_data <- filter(time_series_data, d==duration())
        removeNotification(id)
      }
      # The circuit details file requires no processing and is small so always 
      # load from CSV.
      id <- showNotification("Loading circuit details from csv", duration=1000)
      v$circuit_details <- read.csv(file=circuit_details_file(), header=TRUE, 
                                  stringsAsFactors = FALSE)
      v$circuit_details <- select(v$circuit_details, c_id, site_id, con_type, 
                                  polarity)
      
      v$circuit_details <- v$circuit_details %>% mutate(site_id = as.character(site_id))
      removeNotification(id)
      if (str_sub(site_details_file(), start=-7)=="feather"){
        # If a feather file is used it is assumed the data is pre-processed.
        # Hence the data is not passed to the raw data processor.
        id <- showNotification("Loading site details from csv", duration=1000)
        v$site_details <- read_feather(site_details_file())
        removeNotification(id)
      }else{
        id <- showNotification("Loading site details from csv", duration=1000)
        v$site_details_raw <- read.csv(file=site_details_file(), header=TRUE, 
                                 stringsAsFactors = FALSE)
        v$site_details_raw <- v$site_details_raw %>% mutate(site_id = as.character(site_id))
        removeNotification(id)
        id <- showNotification("Formatting site details and 
                                creating feather cache file", duration=1000)
        # Data from CSV is assumed to need processing.
        v$site_details <- process_raw_site_details(v$site_details_raw)
        # Automatically create a cache of the processed data as a feather file.
        # Allows for much faster data loading for subsequent anaylsis.
        file_no_type = str_sub(site_details_file(), end=-4)
        file_type_feather = paste(file_no_type, "feather", sep="")
        write_feather(v$site_details, file_type_feather)
        removeNotification(id)
      }
      id <- showNotification("Load CER capacity data", duration=1000)
      # Load in the install data from CSV.
      intall_data_file <- "cumulative_capacity_and_number_20190121.csv"
      install_data <- read.csv(file=intall_data_file, header=TRUE, 
                               stringsAsFactors = FALSE)
      v$install_data <- process_install_data(install_data)
      postcode_data_file <- "PostcodesLatLong.csv"
      postcode_data <- read.csv(file=postcode_data_file, header=TRUE, 
                               stringsAsFactors = FALSE)
      postcode_data <- postcode_data %>%
        mutate(postcode = as.character(postcode))
      removeNotification(id)
      # Perform data, processing and combine data table into a single data frame
      id <- showNotification("Combining data tables", duration=1000)
      v$combined_data <- combine_data_tables(time_series_data, 
                                                          v$circuit_details, 
                                                          v$site_details)
      #v$combined_data <- filter(combined_data_no_ac_filter, sum_ac<=100)
      v$combined_data <- v$combined_data %>% mutate(clean="raw")
      v$combined_data <- 
        select(v$combined_data, c_id, ts, v, f, d, site_id, e, con_type,
               s_state, s_postcode, Standard_Version, Grouping, polarity, first_ac,
               power_kW, clean, manufacturer, model, sum_ac)
      removeNotification(id)
      id <- showNotification("Cleaning data", duration=1000)
      #combined_data_no_ac_filter <- 
      #  select(combined_data_no_ac_filter, c_id, ts, e, site_id, con_type,
      #         polarity, first_ac, s_postcode, power_kW)
      # Clean site details data
      print("clean 1")
      site_details_cleaned <- site_details_data_cleaning(
        v$combined_data, v$site_details_raw)
      v$site_details_cleaned <- site_details_cleaned[
        order(site_details_cleaned$site_id),]
      # Clean circuit details file
      print("clean 2")
      v$circuit_details_for_editing <- 
        clean_connection_types(v$combined_data, v$circuit_details, postcode_data)
      #v$combined_data_no_ac_filter <- select(combined_data_no_ac_filter, ts, site_id, c_id, power_kW)
      #remove(combined_data_no_ac_filter)
      # Display data cleaning output in data cleaning tab
      output$site_details_editor <- renderDT(
        isolate(v$site_details_cleaned), selection='single', rownames=FALSE, 
        editable=TRUE)
      v$proxy_site_details_editor <- dataTableProxy('site_details_editor')
      output$circuit_details_editor <- renderDT(
        isolate(v$circuit_details_for_editing), selection='single', 
        rownames=FALSE, editable=TRUE)
      v$proxy_circuit_details_editor <- dataTableProxy('circuit_details_editor')
      
      # Add cleaned data to display data for main tab
      print("clean 3")
      site_details_cleaned_processed <- process_raw_site_details(v$site_details_cleaned)
      combined_data_after_clean <- combine_data_tables(
        time_series_data, 
        v$circuit_details_for_editing, 
        site_details_cleaned_processed)
      remove(time_series_data)
      combined_data_after_clean <- filter(combined_data_after_clean, sum_ac<=100)
      v$combined_data <- filter(v$combined_data, clean=="raw")
      combined_data_after_clean <- combined_data_after_clean %>% mutate(clean="cleaned")
      combined_data_after_clean <- select(combined_data_after_clean, 
                                          c_id, ts, v, f, d, site_id, e, con_type,
                                          s_state, s_postcode, Standard_Version, Grouping, polarity, first_ac,
                                          power_kW, clean, manufacturer, model, sum_ac)
      v$combined_data <- rbind(v$combined_data, combined_data_after_clean)
      remove(combined_data_after_clean)
      removeNotification(id)
      # Filtering option widgets are rendered after the data is loaded, this is 
      # because they are only usable once there is data to filter. Additionally
      # The data loaded can then be used to create the appropraite options for 
      # filtering.
      output$cleaned <- renderUI({
        checkboxGroupButtons(inputId="cleaned", 
                             label=strong("Data processing:"),
                             choices=list("cleaned", "raw"),
                             selected=list("cleaned"),
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"), 
                                            no=icon("remove", lib="glyphicon")))
      })
      output$dateWidget <- renderUI({
        dateRangeInput("date", label=strong('Date range (yyyy-mm-dd):'),
                       start=floor_date(min(v$combined_data$ts), "day"),
                       end=floor_date(max(v$combined_data$ts), "day"),
                       min=floor_date(min(v$combined_data$ts), "day"),
                       max=floor_date(max(v$combined_data$ts), "day"), 
                       startview="year")
        })
      output$time_start <- renderUI({
        timeInput("time_start", label=strong('Enter start time'),
                  value = as.ITime(min(v$combined_data$ts)))
        })
      output$time_end <- renderUI({
        timeInput("time_end", label=strong('Enter end time'), 
                  value=as.ITime(max(v$combined_data$ts)))
        })
      output$region <- renderUI({
        selectInput(inputId="region", label=strong("Region"), 
                    choices=unique(v$combined_data$s_state))
        })
      #output$duration <- renderUI({
      #  radioButtons("duration", 
      #               label=strong("Sampled duration (seconds), select one"),
      #               choices = list("5","30","60"), selected = "60", 
      #               inline = TRUE)})
      output$postcodes <- renderUI({
        selectizeInput("postcodes", 
                      label=strong("Select postcodes"),
                      choices = unique(v$combined_data$s_postcode), 
                      multiple=TRUE)  
        })
      output$manufacturers <- renderUI({
        selectizeInput("manufacturers", 
                       label=strong("Select manufacturers"),
                       choices = unique(v$combined_data$manufacturer), 
                       multiple=TRUE)  
      })
      output$models <- renderUI({
        selectizeInput("models", 
                       label=strong("Select models"),
                       choices = unique(v$combined_data$model), 
                       multiple=TRUE)  
      })
      show("Std_Agg_Indiv")
      output$size_groupings <- renderUI({
        checkboxGroupButtons(inputId="size_groupings", 
                             label=strong("Size Groupings"),
                             choices=list("30-100kW", "<30 kW"),
                             selected=list("30-100kW", "<30 kW"),
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"), 
                                            no=icon("remove", lib="glyphicon")))
      })
      output$StdVersion <- renderUI({
        checkboxGroupButtons(inputId="StdVersion", 
                             label=strong("AS47777 Version:"),
                             choices=list("AS4777.3:2005", "Transition", 
                                          "AS4777.2:2015"),
                             selected=list("AS4777.3:2005", "Transition", 
                                           "AS4777.2:2015"),
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"), 
                                            no=icon("remove", lib="glyphicon")))
        })
      show("raw_upscale")
      show("pst_agg")
      show("grouping_agg")
      show("manufacturer_agg")
      show("model_agg")
      output$event_date <- renderUI({
        dateInput("event_date", label=strong('Event date (yyyy-mm-dd):'),
                  value=floor_date(min(v$combined_data$ts), "day"),
                  startview="year")
      })
      output$event_time <- renderUI({
        timeInput("event_time", label=strong('Event time'),
                  value = as.ITime(min(v$combined_data$ts)))
      })
      output$update_plots <- renderUI({
        actionButton("update_plots", "Update plots")
        })
    }, warning = function(war) {
      shinyalert("Opps", paste("",war))
    }, error = function(err) {
      shinyalert("Opps", paste("",err))
    }, finally = {
      removeNotification(id)
    })
  })

  # Create plots when update plots button is clicked.
  observeEvent(input$update_plots, {
    id <- showNotification("Updating plots", duration=1000)
    # Filter data based on user selections
  #  combined_data_f <- vector_filter(filter(v$combined_data, sum_ac<=100), 
  #                                   duration=duration(), 
  #                                   state=region(), standards=standards(),
  #                                   clean=clean(), postcodes=postcodes(),
  #                                   size_groupings=size_groupings(),
  #                                   manufacturers=manufacturers(),
  #                                   models=models())
    
    combined_data_f <- filter(v$combined_data, sum_ac<=100)
    combined_data_f <- filter(combined_data_f, s_state==region())
    if (length(clean()) < 2) {combined_data_f <- filter(combined_data_f, clean %in% clean())}
    if (length(size_groupings()) < 2) {combined_data_f <- filter(combined_data_f, Grouping %in% size_groupings())}
    if (length(standards()) < 3) {combined_data_f <- filter(combined_data_f, Standard_Version %in% standards())}
    if (length(postcodes()) > 0) {combined_data_f <- filter(combined_data_f, s_postcode %in% postcodes())}
    if (length(manufacturers()) > 0) {combined_data_f <- filter(combined_data_f, manufacturer %in% manufacturers())}
    if (length(models()) > 0) {combined_data_f <- filter(combined_data_f, model %in% models())}
    #combined_data_f <- filter(combined_data_f, d==duration())
    
    # Calculate normalised power curves
    et <- event_time()
    combined_data_f <- event_normalised_power(combined_data_f, et)
    # Filter data by user selected time window
    combined_data_f <- filter(combined_data_f, 
                              ts>=start_time() & ts<= end_time())
    # Copy data for saving
    v$combined_data_f <- select(combined_data_f, ts, v, f, site_id,
                                s_state, s_postcode, Standard_Version, Grouping, 
                                first_ac, power_kW, clean, manufacturer, model)

    # Count samples in each data series to be displayed
    v$sample_count_table <- vector_groupby_count(combined_data_f, 
                                                 agg_on_standard=agg_on_standard(),
                                                 pst_agg=pst_agg(), 
                                                 grouping_agg=grouping_agg(),
                                                 manufacturer_agg=manufacturer_agg(),
                                                 model_agg=model_agg())
    # Create copy of filtered data to use in upscaling
    combined_data_f2 <- combined_data_f
    if(raw_upscale()){
      combined_data_f2 <- upscale(combined_data_f2, v$install_data)
    }
    
    # Check that the filter does not result in an empty dataframe.
    if(length(combined_data_f$ts) > 0){
      # For first plot just aggregate filtered data.
      agg_norm_power <- vector_groupby_norm_power(combined_data_f, 
                                    agg_on_standard=agg_on_standard(),
                                    pst_agg=pst_agg(), 
                                    grouping_agg=grouping_agg(),
                                    manufacturer_agg=manufacturer_agg(),
                                    model_agg=model_agg())
      v$agg_power <- vector_groupby_power(combined_data_f2, 
                                    agg_on_standard=agg_on_standard(),
                                    pst_agg=pst_agg(), 
                                    grouping_agg=grouping_agg(),
                                    manufacturer_agg=manufacturer_agg(),
                                    model_agg=model_agg())
      v$agg_power <- left_join(v$agg_power, 
                               agg_norm_power[, c("Event_Normalised_Power_kW", "Frequency", "Voltage", "series", "Time")], 
                               by=c("Time", "series"))
      output$PlotlyTest <- renderPlotly({
        plot_ly(v$agg_power, x=~Time, y=~Power_kW, color=~series, 
                type="scatter")})
      output$save_agg_power <- renderUI({
        shinySaveButton("save_agg_power", "Save Aggregated Results", "Save file as ...", 
                        filetype=list(xlsx="csv"))})
      output$save_underlying <- renderUI({
        shinySaveButton("save_underlying", "Save Underlying Data", "Save file as ...", 
                        filetype=list(xlsx="csv"))})
      
      output$sample_count_table <- renderDataTable({v$sample_count_table})
      output$save_sample_count <- renderUI({
        shinySaveButton("save_sample_count", "Save data", "Save file as ...", 
                        filetype=list(xlsx="csv"))})
      output$NormPower <- renderPlotly({
        plot_ly(agg_norm_power, x=~Time, y=~Event_Normalised_Power_kW, 
                color=~series, type="scatter")})
      output$Frequency <- renderPlotly({
        plot_ly(agg_norm_power, x=~Time, y=~Frequency, 
                color=~series, type="scatter")})
      output$Voltage <- renderPlotly({
        plot_ly(agg_norm_power, x=~Time, y=~Voltage, 
                color=~series, type="scatter")})

      
      removeNotification(id)
    } else {
      # If there is no data left after filtering alert the user and create an
      # empty plot.
      shinyalert("Opps", "There is no data to plot")
      output$PlotlyTest <- renderPlotly({})
      removeNotification(id)
    }
  })
  
  # Save data from aggregate pv power plot
  observe({
    volumes <- c(dr="C:\\")
    shinyFileSave(input, "save_agg_power", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_agg_power)
    if (nrow(fileinfo) > 0) {
      write.csv(v$agg_power, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  # Save data from aggregate pv power plot
  observe({
    volumes <- c(dr="C:\\")
    shinyFileSave(input, "save_underlying", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_underlying)
    if (nrow(fileinfo) > 0) {
      id <- showNotification("Saving Underlying Data at Site Level", duration=1000)
      write.csv(v$combined_data_f, as.character(fileinfo$datapath), row.names=FALSE)
      removeNotification(id)
    }
  })
  
  # Save data from sample count table
  observe({
    volumes <- c(dr="C:\\")
    shinyFileSave(input, "save_sample_count", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save_sample_count)
    if (nrow(fileinfo) > 0) {
      write.csv(v$sample_count_table, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  # Time series file selection pop up.
  observe({
    volumes <- c(dr="C:\\")
    shinyFileChoose(input, "choose_ts", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_ts)
    if (nrow(fileinfo) > 0) {
      updateTextInput(session, "time_series", value=as.character(fileinfo$datapath))
    }
  })
  
  # Circuit details file selection pop up.
  observe({
    volumes <- c(dr="C:\\")
    shinyFileChoose(input, "choose_c", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_c)
    if (nrow(fileinfo) > 0) {
      updateTextInput(session, "circuit_details", value=as.character(fileinfo$datapath))
    }
  })
  
  # Site details file selection pop up.
  observe({
    volumes <- c(dr="C:\\")
    shinyFileChoose(input, "choose_site", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_site)
    if (nrow(fileinfo) > 0) {
      updateTextInput(session, "site_details", value=as.character(fileinfo$datapath))
    }
  })
  
  # Inforce mutual exclusivity of Aggregation settings
  observe({
    if(manufacturer_agg() | model_agg() | pst_agg()){
      updateMaterialSwitch(session=session, "raw_upscale", value = FALSE)
    }
  })
  observe({
    if(raw_upscale()){
      updateMaterialSwitch(session=session, "manufacturer_agg", value = TRUE)
    }
  })
  observe({
    if(raw_upscale()){
      updateMaterialSwitch(session=session, "model_agg", value = TRUE)
    }
  })
  observe({
    if(raw_upscale()){
      updateMaterialSwitch(session=session, "pst_agg", value = TRUE)
    }
  })
  
  observeEvent(input$circuit_details_editor_rows_selected, {
    v$proxy_site_details_editor %>% selectRows(NULL)
    if (length(input$circuit_details_editor_rows_selected==1)) {
      c_id_to_plot <- v$circuit_details_for_editing$c_id[input$circuit_details_editor_rows_selected]
      data_to_view <- filter(filter(v$combined_data, clean=="raw"), c_id==c_id_to_plot)
      output$site_plot <- renderPlotly({
        plot_ly(data_to_view, x=~ts, y=~power_kW, type="scatter")})
    }
    
  })
  
  # Plot the data for the site or circuit selected in the table
  observeEvent(input$site_details_editor_rows_selected, {
    v$proxy_circuit_details_editor %>% selectRows(NULL)
    if (length(input$site_details_editor_rows_selected==1)) {
      site_id_to_plot <- v$site_details_cleaned$site_id[input$site_details_editor_rows_selected]
      data_to_view <- filter(filter(v$combined_data, clean=="raw"), site_id==site_id_to_plot)
      output$site_plot <- renderPlotly({
        plot_ly(data_to_view, x=~ts, y=~power_kW, type="scatter")})
    }
  })
  
  # Save table data to csv.
  observeEvent(input$save_cleaned_data, {
    # Save cleaned data to csv
    id <- showNotification("Saving cleaned files", duration=1000)
    file_no_type = str_sub(site_details_file(), end=-5)
    new_file_name = paste(file_no_type, "_cleaned.csv", sep="")
    v$site_details_cleaned <- v$site_details_cleaned %>% mutate(
      site_id=as.character(site_id))
    write.csv(v$site_details_cleaned, new_file_name)
    file_no_type = str_sub(circuit_details_file(), end=-5)
    new_file_name = paste(file_no_type, "_cleaned.csv", sep="")
    write.csv(v$circuit_details_for_editing, new_file_name)
    # Add cleaned data to dispay dat set
    site_details_cleaned_processed <- process_raw_site_details(
      v$site_details_cleaned)
    time_series_data <- read_feather(time_series_file())
    time_series_data <- filter(time_series_data, d==duration())
    combined_data_after_clean <- combine_data_tables(
      time_series_data, v$circuit_details_for_editing, 
      site_details_cleaned_processed)
    combined_data_after_clean <- filter(combined_data_after_clean, 
                                          sum_ac<=100)
    v$combined_data <- filter(v$combined_data, clean=="raw")
    combined_data_after_clean <- combined_data_after_clean %>% mutate(clean="cleaned")
    combined_data_after_clean <- select(combined_data_after_clean, 
                                        c_id, ts, v, f, d, site_id, e, con_type,
                                        s_state, s_postcode, Standard_Version, Grouping, polarity, first_ac,
                                        power_kW, clean, manufacturer, model, sum_ac)
    v$combined_data <- rbind(v$combined_data, combined_data_after_clean)
    removeNotification(id)
  })
  
  observeEvent(input$site_details_editor_cell_edit, {
    info = input$site_details_editor_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    value = info$value
    v$site_details_cleaned[i, j] <<- DT::coerceValue(value, v$site_details_cleaned[i, j])
    replaceData(v$proxy_site_details_editor, v$site_details_cleaned, 
                resetPaging=FALSE, rownames=FALSE)  # important
  })
  
  observeEvent(input$circuit_details_editor_cell_edit, {
    info = input$circuit_details_editor_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    value = info$value
    v$circuit_details_for_editing[i, j] <<- DT::coerceValue(value, v$circuit_details_for_editing[i, j])
    replaceData(v$proxy_circuit_details_editor, v$circuit_details_for_editing, 
                resetPaging=FALSE, rownames=FALSE)  # important
  })
}

shinyApp(ui = ui, server = server)

