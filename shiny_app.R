require(tidyverse)
require(shiny)
require(shinydashboard)
require(shinyBS)
require(hrbrthemes)
require(forecast)
require(transformr)
require(grid)
require(gridExtra)
require(RColorBrewer)
require(usmap)

source("data_preparation.R")
source("shiny_functions.R")

# If dataset has not been created for shiny app, create and load in most recent COVID data. 

if(file.exists("data/va_covid")){
  
  va_covid <- read_rds("data/va_covid")
  
}else{
  
  load_covid_data()
  va_covid <- read_rds("data/va_covid")
  
}


# Shiny app

body <- dashboardBody(
  
  
  fluidRow(
    
    # User filters COVID data by date, district, statistic, and demographic
    
    box(
      title = textOutput("intro"),
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
    
    # Output trend-lines
    
    box(
      title = "Cumulative Timeseries, Stacked by Selected Demographic",
      width = 5,
      plotOutput("timeseries")
    ),
    
    # Output table and map-plot
    
    box(
      width = 5,
      align = "center",
      title = "District-Level Breakdown",
      footer = "Blue dot shows currently selected health district (Point sizes are relative to population level)",
      fluidRow(
        tableOutput("demo_table")
      ),
      fluidRow(
        plotOutput("demo_breakdown", height = "40%")
      )
    )
  ),
  
    
  fluidRow(
    
    # User specifies ARIMA model parameters
    
    tabBox(
      title = textOutput("analysis"),
      width = 2,

      # Non-seasonal ARIMA parameters
      
      tabPanel(
        "Non-Seasonal", NULL,
        sliderInput("p", "Lag Degree (p)",
                    min = 0, max = 2,
                    value = 0),
        bsTooltip("p", "Select the number of data points from previous days to include in your model.",
                  "right", options = list(container = "body")),
        sliderInput("d", "Order of Differencing (d)",
                    min = 0, max = 2,
                    value = 0),
        bsTooltip("d", "Select the times to difference the data.",
                  "right", options = list(container = "body")),
        sliderInput("q", "Number of Moving Average Terms (q)",
                    min = 0, max = 2,
                    value = 0),
        bsTooltip("q", "Select the number of error-terms from previous days to include in your model.",
                  "right", options = list(container = "body"))
      ),
      
      # Seasonal ARIMA parameters
      
      tabPanel(
        "Seasonal", NULL,
        selectInput("period", "Seasonal Period", 
                    choices = list("Weekly" = 7, "Bi-Weekly" = 14, 
                                   "Monthly" = 30, "Bi-Monthly" = 60)),
        bsTooltip("period", "Select the window size of your seasonal parameters to see if a periodic structure exists.",
                  "right", options = list(container = "body")),
        sliderInput("sp", "Lag Degree (p)",
                    min = 0, max = 2,
                    value = 0),
        bsTooltip("sp", "Select the number of data points from previous days to include in your model.",
                  "right", options = list(container = "body")),
        sliderInput("sd", "Order of Differencing (d)",
                    min = 0, max = 2,
                    value = 0),
        bsTooltip("sd", "Select the times to difference the data.",
                  "right", options = list(container = "body")),
        sliderInput("sq", "Number of Moving Average Terms (q)",
                    min = 0, max = 2,
                    value = 0),
        bsTooltip("sq", "Select the number of error-terms from previous days to include in your model.",
                  "right", options = list(container = "body"))
        
      )
    ),
    
    # Output ARIMA fit
    
    box(
      title = "ARIMA Fitting and Forecast of Next 2 Weeks",
      width = 5,
      plotOutput("arima")
    ),
    
    # Output ARIMA diagnostics
    
    box(
      title = "ARIMA Diagnostic Plots",
      width = 5,
      plotOutput("diag")
    )
  )
  
)

# Create shiny UI

ui <-  dashboardPage(
  skin = "red",
  dashboardHeader(titleWidth = 1000, title = "Virginia COVID-19 Analysis Application - Danielle Sebring,
                  Ankur Patel, and Andrew Cooper"),
  dashboardSidebar(disable = T),
  body
)

# Create shiny server

server <- function(input, output, session){
  
  # Reactive values for trend data, ARIMA model-specific data, and ARIMA fit
  
  state <- reactiveValues(
    trend_data = list(),
    arima_data = list(),
    model_fit  = list()
  )
  
  
  # If user changes any parameters, recreate plots and ARIMA model fitting
  
  observe({
    state$trend_data <- get_numbers(va_covid, input$date[1], input$date[2], 
                                    input$district, input$demo, input$stat)
    state$arima_data <- state$trend_data %>%   
      group_by(date) %>% 
      summarize(sum(count)) %>% 
      mutate(`sum(count)` = c(0, diff(`sum(count)`)))
    state$model_fit  <- fit_arima(state$arima_data, 
                                  input$p, input$d, input$q, 
                                  input$sp, input$sd, input$sq, as.numeric(input$period))
  })
  
  
  # Create outputs for shiny app
  
  output$timeseries <- renderPlot({
    plot_timeseries(state$trend_data)
  })
  
  output$demo_table <- renderTable({
    summary_given_district(va_covid, input$district)
  })
  
  output$demo_breakdown <- renderPlot(
    height = 250,
    width  = 500,
    {
      plot_demographics(va_covid, input$district)
    }
  )
  
  output$arima <- renderPlot({
    plot_arima(state$arima_data, state$model_fit, input$date[2], 
               input$p, input$d, input$q,
               input$sp, input$sd, input$sq, input$period)
  })
  
  # Add descriptions of outputs user can view by hovering over with their mouse
  
  addPopover(session, "arima", "ARIMA Forecasting",
             "The red line shows the fitted values according to the current ARIMA model. In addition it provides
             a two-week forecast with an orange band representing the 95% confidence interval of the forecast.
             The goal is to create a model that captures the general trend of the data but doesn't 'overfit' it.",
             "top", options = list(container = "body"))
  
  output$diag <- renderPlot({
    plot_diagnostics(state$model_fit)
  })
  
  addPopover(session, "diag", "ARIMA Diagnostics", 
             "The residuals plot shows how well the current ARIMA model fits the data. 
                The Auto-correlation (ACF) plot shows the correlation of each residual with all prior values.
                The Partial Auto-correlation (PACF) plot shows the correlation of each residual with only the 
             previous value. The goal is to choose a model that has small, random-looking residuals and 
             'de-correlates' the data.",
             "top", options = list(container = "body"))
  
  output$intro <- renderText({"COVID-19 Trends by Demographic"})
  
  addPopover(session, "intro", "VA Covid Analysis Application",
             "This dashboard presents interactive visualizations of COVID-19 data from the Virginia Department of 
             Health, updated on a daily basis (source: https://www.vdh.virginia.gov/coronavirus/). ",
             "right", options = list(container = "body"))
  
  output$analysis <- renderText({"ARIMA Model-Fitting"})
  
  addPopover(session, "analysis", "ARIMA Model-Fitting",
             "'ARIMA' stands for 'Auto-Regressive Integrated Moving Average', named for its three components 'AR' 
             (denoted by the letter 'p'), 'I' (denoted by the letter 'q', and 'MA' (denoted by the letter 'q')).
             It is a modeling approach frequently applied to time-series data due to its often highly correlated data.",
             "right", options = list(container = "body"))
  
}

shinyApp(ui = ui, server = server)
