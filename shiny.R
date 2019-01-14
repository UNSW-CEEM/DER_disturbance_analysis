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
source("data_manipulation_functions.R")

ui <- fluidPage(
  #Title of Page
  titlePanel("PV System Disturbance Analysis"),
  #Input Bar
  sidebarLayout(
    sidebarPanel(
      textInput("time_series", "Time series file", 
                value="data/example/time_series_subset.csv"),
      textInput("circuit_details", "Circuit details file", 
                value="data/example/circuit_details.csv"),
      textInput("site_details", "Site details file", 
                value="data/example/site_details.csv"),
      actionButton("load_data", "Load data"),
      uiOutput("dateWidget"),
      uiOutput("time_start"),
      uiOutput("time_end"),
      uiOutput("region"),
      uiOutput("duration"),
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
  
  # Get input from GUI
  time_series_file <- reactive({input$time_series})
  circuit_details_file <- reactive({input$circuit_details})
  site_details_file <- reactive({input$site_details})
  region <- reactive({input$region})
  duration <- reactive({input$duration})
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
    result = tryCatch({
      # Load data from CSVs.
      time_series_data <- read.csv(file=time_series_file(), header=TRUE, 
                                   stringsAsFactors = FALSE)
      circuit_details <- read.csv(file=circuit_details_file(), header=TRUE, 
                                  stringsAsFactors = FALSE)
      site_details <- read.csv(file=site_details_file(), header=TRUE, 
                               stringsAsFactors = FALSE)
      # Perform data, processing and combine data table into a single data frame
      v$combined_data <- combine_data_tables(time_series_data, circuit_details, 
                                             site_details)
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
      output$update_plots <- renderUI({
        actionButton("update_plots", "Update plots")
        })
    }, warning = function(war) {
      shinyalert("Opps", paste("",war))
    }, error = function(err) {
      shinyalert("Opps", paste("",err))
    }, finally = {
    })
  })

  observeEvent(input$update_plots, {
    # Filter data based on user selections
    combined_data_f <- filter(v$combined_data, d==duration())
    combined_data_f <- filter(combined_data_f, s_state==region())
    combined_data_f <- filter(combined_data_f, 
                              ts>=start_time() & ts<= end_time())
    # Check that the filter does not result in an empty dataframe.
    if (length(combined_data_f$ts) > 0){
      # For first plot just aggregate filtered data.
      agg_power <- aggregate(combined_data_f$power_kW, 
                             by=list(Category=combined_data_f$ts), FUN=sum)
      v$agg_power <- setnames(agg_power, c("Category", "x"), 
                            c("Time", "Power_kW"))
      output$PlotlyTest <- renderPlotly({ 
        plot_ly(v$agg_power, x=~Time, y=~Power_kW, type="scatter")})
      output$save_agg_power <- renderUI({
        shinySaveButton("save", "Save data", "Save file as ...", filetype=list(xlsx="csv"))
        })
    } else {
      # If there is no data left after filtering alert the user and create an
      # empty plot.
      shinyalert("Opps", "There is no data to plot")
      output$PlotlyTest <- renderPlotly({})
    }
  })
  
  observe({
    volumes <- c("wd"=getwd())
    shinyFileSave(input, "save", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save)
    if (nrow(fileinfo) > 0) {
      write.csv(v$agg_power, as.character(fileinfo$datapath), row.names=FALSE)
    }
  })

}

shinyApp(ui = ui, server = server)

