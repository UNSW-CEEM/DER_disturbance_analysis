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
  
  v <- reactiveValues(combined_data = data.frame())
  
  observeEvent(input$load_data, {
    print("Button clicked")
    time_series_data <- read.csv(file=time_series_file(), header=TRUE, stringsAsFactors = FALSE)
    circuit_details <- read.csv(file=circuit_details_file(), header=TRUE, stringsAsFactors = FALSE)
    site_details <- read.csv(file=site_details_file(), header=TRUE, stringsAsFactors = FALSE)
    v$combined_data <- combine_data_tables(time_series_data, circuit_details, site_details, min(time_series_data$ts), 
                                           max(time_series_data$ts))
    output$dateWidget <- renderUI({
      dateRangeInput("date", label = strong('Date range (yyyy-mm-dd):'),start = min(v$combined_data$ts),
                     end = floor_date(max(v$combined_data$ts), "day"),
                     min = min(v$combined_data$ts),
                     max = floor_date(max(v$combined_data$ts), "day"), startview = "year")
    })
    output$time_start <- renderUI({timeInput("time_start", label =strong('Enter start time'),value = as.ITime(min(v$combined_data$ts)))})
    output$time_end <- renderUI({timeInput("time_end", label=strong('Enter end time'),value = as.ITime(max(v$combined_data$ts)))})
    output$region <- renderUI({selectInput(inputId="region", label = strong("Region"), choices=unique(v$combined_data$s_state))})
    output$duration <- renderUI({radioButtons("duration", label = strong("Sampled duration (seconds), please select one"),
                 choices = list("5","30","60"), selected = "60", inline = TRUE)})
    output$update_plots <- renderUI({actionButton("update_plots", "Update plots")})

  })

  observeEvent(input$update_plots, {
    print("trying to plot")
    combined_data_f <- filter(v$combined_data, d==duration())
    combined_data_f <- filter(combined_data_f, s_state==region())
    if (length(combined_data_f$ts) > 0){
      agg_power <- aggregate(combined_data_f$power_kW, by=list(Category=combined_data_f$ts), FUN=sum)
      output$PlotlyTest <- renderPlotly({ plot_ly(agg_power, x=~Category, y=~x, type="scatter")})
    } else {
      shinyalert("Opps", "There is no data to plot with you current filter settings")
      output$PlotlyTest <- renderPlotly({})
    }
  })

}

shinyApp(ui = ui, server = server)

