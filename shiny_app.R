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


va_covid <- read_rds("data/va_covid")


# Shiny functions for data manipulation and plotting

get_numbers <- function(df, start, end, hd, demo, stat){
  df_filtered <- df %>%
    filter(date >= start, date <= end, 
           health_district == hd,
           demographic == demo,
           statistic == stat)
  return(df_filtered)
}


plot_timeseries <- function(df){
  df %>% 
    ggplot(aes(x = date, y = count, fill = level)) + 
    geom_area(alpha = 0.8)
}


fit_arima <- function(arima_data, stat, p, d, q){
  arima_fit <- Arima(arima_data %>% .$"sum(count)", order = c(p, d, q))
  p1 <- arima_data %>% 
    mutate(count = c(0, `sum(count)` %>% diff())) %>% 
    ggplot(aes(x = date, y = count)) + 
    geom_line()
  p2 <- ggAcf(residuals(arima_fit))
  grid.arrange(p1, p2, nrow = 2)
}


# Shiny app

body <- dashboardBody(
  fluidRow(
    box(width = 3,
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
    box(width = 5,
      plotOutput("timeseries")
    ),
    box(width = 4,
        imageOutput("image1"))
  ),
  fluidRow(
    box(width = 3,
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
    box(width = 5,
        plotOutput("arima")
    )
  )
)

ui <-  dashboardPage(
  skin = "red",
  dashboardHeader(titleWidth = 450, title = "Virginia COVID-19 Analysis Application"),
  dashboardSidebar(),
  body
)

server <- function(input, output, session){
  
  state <- reactiveValues(
    trend_data = list(),
    arima_data = list()
  )
  
  observe({
    state$trend_data <- get_numbers(va_covid, input$date[1], input$date[2], input$district, input$demo, input$stat)
    state$arima_data <- state$trend_data %>%   
      group_by(date) %>% 
      summarize(sum(count))
  })
  
  output$timeseries <- renderPlot({
    plot_timeseries(state$trend_data)
  })
  
  output$arima <- renderPlot({
    fit_arima(state$arima_data, input$stat, input$p, input$d, input$q)
  })
  
  output$image1 <- renderImage({
    return(list(src = "gganim.gif", height = "400px", width = "500px"))
  })
}

shinyApp(ui = ui, server = server)
