library(tidyverse)
library(shiny)
library(gganimate)
library(hrbrthemes)
library(gifski)
library(forecast)
library(transformr)
library(animation)
library(shinydashboard)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(usmap)

va_covid <- read_rds("va_covid")

source("shiny_functions.R")


# Shiny app

body <- dashboardBody(
  
  fluidRow(
    
    box(
      title = "COVID-19 Trends by Demographic",
      width = 2,
      dateRangeInput("date", "Date Range", 
                     start = min(va_covid$date), end = max(va_covid$date),
                     min = min(va_covid$date), max = max(va_covid$date)),
      selectInput("district", "Health District", 
                  choices = va_covid$health_district %>% unique() %>% sort()),
      selectInput("stat", "Statistic",
                  choices = va_covid$statistic %>% unique() %>% sort()),
      radioButtons("demo", "Demographic", 
                   choices = va_covid$demographic %>% unique())
    ),
    
    box(title = "Timeseries Stacked by Demographic",
        width = 5,
        plotOutput("timeseries")
    ),
    
    box(
      title = "Red dot represents currently selected health district",
      width = 5,
      plotOutput("demo_breakdown"))
  ),
  
  fluidRow(
    
    tabBox(title = "ARIMA Model-Fitting",
           width = 2,
           tabPanel("Non-Seasonal", "Non-Seasonal Parameters",
                    sliderInput("p", "Lag Degree (p)",
                                min = 0, max = 3,
                                value = 0),
                    sliderInput("d", "Order of Differencing (d)",
                                min = 0, max = 3,
                                value = 0),
                    sliderInput("q", "Number of Moving Average Terms (q)",
                                min = 0, max = 3,
                                value = 0)
           ),
           
           tabPanel("Seasonal", "Seasonal Parameters",
                    selectInput("period", NULL, 
                                choices = list("Weekly" = 7, "Bi-Weekly" = 14, 
                                               "Monthly" = 30, "Bi-Monthly" = 60,
                                               "Tri-Monthly" = 90),
                                selected = 30),
                    sliderInput("sp", "Lag Degree (p)",
                                min = 0, max = 3,
                                value = 0),
                    sliderInput("sd", "Order of Differencing (d)",
                                min = 0, max = 3,
                                value = 0),
                    sliderInput("sq", "Number of Moving Average Terms (q)",
                                min = 0, max = 3,
                                value = 0)
           )
    ),
    
    box(title = "ARIMA Forecast",
        width = 5,
        plotOutput("arima")
    ),
    
    box(title = "ARIMA Diagnostic Plots",
        width = 5,
        plotOutput("diag"))
  )
  
)

ui <-  dashboardPage(
  skin = "red",
  dashboardHeader(titleWidth = 450, title = "Virginia COVID-19 Analysis Application"),
  dashboardSidebar(disable = T),
  body
)

server <- function(input, output, session){
  
  state <- reactiveValues(
    trend_data = list(),
    arima_data = list(),
    model_fit  = list()
  )
  
  
  
  observe({
    state$trend_data <- get_numbers(va_covid, input$date[1], input$date[2], 
                                    input$district, input$demo, input$stat)
    state$arima_data <- state$trend_data %>%   
      group_by(date) %>% 
      summarize(sum(count)) %>% 
      mutate(`sum(count)` = c(0, diff(`sum(count)`)))
    state$model_fit  <- Arima(state$arima_data %>% .$"sum(count)", 
                              order = c(input$p, input$d, input$q), 
                              seasonal = list(order = c(input$sp, input$sd, input$sq), 
                                              period = as.numeric(input$period)))
  })
  

  
  output$timeseries <- renderPlot({
    plot_timeseries(state$trend_data)
  })
  
  output$demo_breakdown <- renderPlot({
    plot_demographics(state$trend_data, input$district, va_covid)
  })
  
  output$arima <- renderPlot({
    plot_arima(state$arima_data, state$model_fit, input$date[2])
  })
  
  output$diag <- renderPlot({
    plot_diagnostics(state$model_fit)
  })
  
  output$image1 <- renderImage({
    return(list(src = "gganim.gif", height = "400px", width = "600px"))
  })
}

shinyApp(ui = ui, server = server)
