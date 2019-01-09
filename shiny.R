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
source("data_manipulation_functions.R")

ui <- fluidPage(
  #Title of Page
  titlePanel("PV System Disturbance Analysis"),
  #Input Bar
  sidebarLayout(
    sidebarPanel(
      textInput("time_series", "Time series file", value="data/example/time_series_subset.csv"),
      textInput("circuit_details", "Circuit details file", value="data/example/circuit_details.csv"),
      textInput("site_details", "Site details file", value="data/example/site_details.csv"),
      actionButton("load_data", "Load data"),
      br(),
      uiOutput("dateWidget"),
      uiOutput("time_start"),
      uiOutput("time_end"),
      uiOutput("region"),
      uiOutput("duration"),
      uiOutput("update_plots")
      ),
    #Output
    mainPanel(
      plotlyOutput(outputId = "PlotlyTest")
    )
    
  ),
  useShinyalert()
)
server <- function(input,output,session){
  
  output$dateWidget <- renderUI({})
  outputOptions(output, "dateWidget", suspendWhenHidden = FALSE)
  
  time_series_file <- reactive({input$time_series})
  circuit_details_file <- reactive({input$circuit_details})
  site_details_file <- reactive({input$site_details})
  region <- reactive({input$region})
  duration <- reactive({input$duration})
  start_time <- reactive({strftime(paste(as.character(input$date[1]), as.character(substr(input$time_start, 12,19))))})
  end_time <- reactive({strftime(paste(as.character(input$date[2]), as.character(substr(input$time_end, 12,19))))})
  
  v <- reactiveValues(combined_data = data.frame())
  
  observeEvent(input$load_data, {
    
    result = tryCatch({
      time_series_data <- read.csv(file=time_series_file(), header=TRUE, stringsAsFactors = FALSE)
      circuit_details <- read.csv(file=circuit_details_file(), header=TRUE, stringsAsFactors = FALSE)
      site_details <- read.csv(file=site_details_file(), header=TRUE, stringsAsFactors = FALSE)
      v$combined_data <- combine_data_tables(time_series_data, circuit_details, site_details)
      output$dateWidget <- renderUI({
        dateRangeInput("date", label = strong('Date range (yyyy-mm-dd):'),start = floor_date(min(v$combined_data$ts), "day"),
                       end = floor_date(max(v$combined_data$ts), "day"),
                       min = floor_date(min(v$combined_data$ts), "day"),
                       max = floor_date(max(v$combined_data$ts), "day"), startview = "year")
      })
      output$time_start <- renderUI({timeInput("time_start", label =strong('Enter start time'),value = as.ITime(min(v$combined_data$ts)))})
      output$time_end <- renderUI({timeInput("time_end", label=strong('Enter end time'),value = as.ITime(max(v$combined_data$ts)))})
      output$region <- renderUI({selectInput(inputId="region", label = strong("Region"), choices=unique(v$combined_data$s_state))})
      output$duration <- renderUI({radioButtons("duration", label = strong("Sampled duration (seconds), please select one"),
                                                choices = list("5","30","60"), selected = "60", inline = TRUE)})
      output$update_plots <- renderUI({actionButton("update_plots", "Update plots")})
    }, warning = function(war) {
      shinyalert("Opps", paste("",war))
      v$combined_data <- data.frame()
    }, error = function(err) {
      shinyalert("Opps", paste("",err))
    }, finally = {
    })
  })

  observeEvent(input$update_plots, {
    print("trying to plot")
    combined_data_f <- filter(v$combined_data, d==duration())
    combined_data_f <- filter(combined_data_f, s_state==region())
    combined_data_f <- filter(combined_data_f, ts>=start_time() & ts<= end_time())
    if (length(combined_data_f$ts) > 0){
      agg_power <- aggregate(combined_data_f$power_kW, by=list(Category=combined_data_f$ts), FUN=sum)
      agg_power <- setnames(agg_power, c("Category", "x"), c("Time", "Power_kW"))
      output$PlotlyTest <- renderPlotly({ plot_ly(agg_power, x=~Time, y=~Power_kW, type="scatter")})
    } else {
      shinyalert("Opps", "There is no data to plot")
      output$PlotlyTest <- renderPlotly({})
    }
  })

}

shinyApp(ui = ui, server = server)

