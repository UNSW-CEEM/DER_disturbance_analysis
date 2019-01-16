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
source("data_manipulation_functions.R")
source("filter_and_aggregate.R")

ui <- fluidPage(
  #Title of Page
  useShinyjs(),
  titlePanel("PV System Disturbance Analysis"),
  #Input Bar
  sidebarLayout(
    sidebarPanel(
      textInput("time_series", "Time series file", 
                value="C:/Users/user/Documents/GitHub/DER_disturbance_analysis/test_data/2018-08-25 raw inputs/2018-08-25_sa_qld_naomi.feather"),
      shinyFilesButton("choose_ts", "Choose File", 
                      "Select timeseries data file ...", multiple=FALSE),
      textInput("circuit_details", "Circuit details file", 
                value="C:/Users/user/Documents/GitHub/DER_disturbance_analysis/test_data/2018-08-25 raw inputs/circuit_details.csv"),
      shinyFilesButton("choose_c", "Choose File", 
                       "Select circuit details data file ...", multiple=FALSE),
      textInput("site_details", "Site details file", 
                value="C:/Users/user/Documents/GitHub/DER_disturbance_analysis/test_data/2018-08-25 raw inputs/site_details.feather"),
      shinyFilesButton("choose_site", "Choose File", 
                       "Select site details data file ...", multiple=FALSE),
      br(),
      actionButton("load_data", "Load data"),
      uiOutput("dateWidget"),
      uiOutput("time_start"),
      uiOutput("time_end"),
      uiOutput("region"),
      uiOutput("duration"),
      materialSwitch("Std_Agg_Indiv", label=strong("AS47777 Aggregated:"), 
                     status="primary"),
      uiOutput("StdVersion"),
      uiOutput("update_plots")
      ),
    #Output
    mainPanel(
      plotlyOutput(outputId = "PlotlyTest"),
      uiOutput("save_agg_power")
    )
    
  ),
  useShinyalert()
)
server <- function(input,output,session){
  hide("Std_Agg_Indiv")
  # Get input from GUI
  time_series_file <- reactive({input$time_series})
  circuit_details_file <- reactive({input$circuit_details})
  site_details_file <- reactive({input$site_details})
  region <- reactive({input$region})
  duration <- reactive({input$duration})
  standards <- reactive({input$StdVersion})
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
  agg_on_standard <- reactive({input$Std_Agg_Indiv})

  # Store the main data table in a reactive value so it is accessable outside 
  # the observe event that creates it.
  v <- reactiveValues(combined_data = data.frame(),
                      agg_power = data.frame())
  
  # This is the event that runs when the "Load data" button on the GUI is
  # Clicked. 
  observeEvent(input$load_data, {
    # Users may enter invalid file paths or attempt to load data that creates
    # an error or warning during the data table combing process. The "tryCatch"
    # function catches these, aborts the loading process and reports the error 
    # to the users. Importantly this prevent the app from crashing.
    t0 <- as.numeric(Sys.time())
    print(tracingState())
    result = tryCatch({
      # Load data from CSVs.
      if (str_sub(time_series_file(), start=-7)=="feather"){
        id <- showNotification("Loading timeseries data from feather", duration=1000)
        time_series_data <- read_feather(time_series_file())
        removeNotification(id)
      }else{
        id <- showNotification("Loading timeseries data from csv", duration=1000)
        time_series_data <- read.csv(file=time_series_file(), header=TRUE, 
                                     stringsAsFactors = FALSE)
        removeNotification(id)
        id <- showNotification("Formatting timeseries data and 
                                creating feather cache file", duration=1000)
        time_series_data <- process_raw_time_series_data(time_series_data)
        file_no_type = str_sub(time_series_file(), end=-4)
        file_type_feather = paste(file_no_type, "feather", sep="")
        write_feather(time_series_data, file_type_feather)
        removeNotification(id)
      }
      id <- showNotification("Loading circuit details from csv", duration=1000)
      circuit_details <- read.csv(file=circuit_details_file(), header=TRUE, 
                                  stringsAsFactors = FALSE)
      removeNotification(id)
      if (str_sub(site_details_file(), start=-7)=="feather"){
        id <- showNotification("Loading site details from csv", duration=1000)
        site_details <- read_feather(site_details_file())
        removeNotification(id)
      }else{
        id <- showNotification("Loading site details from csv", duration=1000)
        site_details <- read.csv(file=site_details_file(), header=TRUE, 
                                 stringsAsFactors = FALSE)
        removeNotification(id)
        id <- showNotification("Formatting site details and 
                                creating feather cache file", duration=1000)
        site_details <- process_raw_site_details(site_details)
        file_no_type = str_sub(site_details_file(), end=-4)
        file_type_feather = paste(file_no_type, "feather", sep="")
        write_feather(site_details, file_type_feather)
        removeNotification(id)
      }

      # Perform data, processing and combine data table into a single data frame
      t1 <- as.numeric(Sys.time())
      id <- showNotification("Combining data tables", duration=1000)
      v$combined_data <- combine_data_tables(time_series_data, circuit_details, 
                                             site_details)
      removeNotification(id)
      print(as.numeric(Sys.time()) - t1, digits=15)
      # Filtering option widgets are rendered after the data is loaded, this is 
      # because they are only usable once there is data to filter. Additionally
      # The data loaded can then be used to create the appropraite options for 
      # filtering.
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
      output$duration <- renderUI({
        radioButtons("duration", 
                     label=strong("Sampled duration (seconds), select one"),
                     choices = list("5","30","60"), selected = "60", 
                     inline = TRUE)
        })
      show("Std_Agg_Indiv")
      output$StdVersion <- renderUI({
        checkboxGroupButtons(inputId="StdVersion", 
                             label=strong("AS47777 Version:"),
                             choices=list("AS4777.3:2005", "Transition", 
                                          "AS4777.2:2015"),
                             justified=TRUE, status="primary", individual=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"), 
                                            no=icon("remove", lib="glyphicon")))})
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
    print(as.numeric(Sys.time()) - t0, digits=15)
  })

  # Create plots when update plots button is clicked.
  observeEvent(input$update_plots, {
    id <- showNotification("Updating plots", duration=1000)
    # Filter data based on user selections
    #combined_data_f <- filter(v$combined_data, d==duration())
    combined_data_f <- vector_filter(v$combined_data, duration=duration(), 
                                state=region(), standards=standards())
    combined_data_f <- filter(combined_data_f, 
                              ts>=start_time() & ts<= end_time())
    # Check that the filter does not result in an empty dataframe.
    if (length(combined_data_f$ts) > 0){
      # For first plot just aggregate filtered data.
      v$agg_power <- vector_groupby(combined_data_f, 
                                    agg_on_standard=agg_on_standard())
      output$PlotlyTest <- renderPlotly({
        plot_ly(v$agg_power, x=~Time, y=~Power_kW, color=~series, 
                type="scatter")})
      output$save_agg_power <- renderUI({
        shinySaveButton("save", "Save data", "Save file as ...", 
                        filetype=list(xlsx="csv"))
        })
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
    shinyFileSave(input, "save", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save)
    if (nrow(fileinfo) > 0) {
      write.csv(v$agg_power, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })
  
  observe({
    volumes <- c(dr="C:\\")
    shinyFileChoose(input, "choose_ts", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_ts)
    if (nrow(fileinfo) > 0) {
      updateTextInput(session, "time_series", value=as.character(fileinfo$datapath))
    }
  })
  
  observe({
    volumes <- c(dr="C:\\")
    shinyFileChoose(input, "choose_c", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_c)
    if (nrow(fileinfo) > 0) {
      updateTextInput(session, "circuit_details", value=as.character(fileinfo$datapath))
    }
  })
  
  observe({
    volumes <- c(dr="C:\\")
    shinyFileChoose(input, "choose_site", roots=volumes, session=session)
    fileinfo <- parseFilePaths(volumes, input$choose_site)
    if (nrow(fileinfo) > 0) {
      updateTextInput(session, "site_details", value=as.character(fileinfo$datapath))
    }
  })

}

shinyApp(ui = ui, server = server)

