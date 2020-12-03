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
    geom_area(alpha = 0.8) +
    theme(panel.background = element_blank()) + 
    scale_fill_brewer(palette = "Dark2")
}


plot_demographics <- function(df, district){
  df_summarized <- df %>% 
    group_by(level) %>% 
    summarize(count = count %>% diff())
  va_map_dat <- va_covid %>% 
    group_by(health_district) %>% 
    summarize(lon = mean(longitude),
              lat = mean(latitude),
              pop = mean(total_population)) %>%
    relocate(health_district, .after = last_col()) %>% 
    usmap_transform()
  p <- plot_usmap("counties", include = c("VA")) + 
    geom_point(data = va_map_dat, aes(x = lon.1, y = lat.1, size = pop),
               color = "orange", alpha = 1) +
    geom_point(data = va_map_dat %>% filter(health_district == district), 
               aes(x = lon.1, y = lat.1), 
               size = 10, color = "red") +
    theme(legend.position = "none")
  print(p, vp = viewport(angle = -15))
}


plot_arima <- function(df, model, end_date){
  
  future_dates <- seq(end_date, end_date + 29, by = "day")
  model_forecast <- predict(model, n.ahead = 30)
  forecast_df <- data_frame(mean = model_forecast$pred) %>% 
    mutate(lwr = mean - 1.96*model_forecast$se,
           upr = mean + 1.96*model_forecast$se,
           future_dates = future_dates)
  
  fitted_df <- df %>% 
    mutate(fitted_vals = fitted(model)) %>% 
    gather(type, value, `sum(count)`, fitted_vals)
  
  p <- ggplot(data = fitted_df, aes(x = date, y = value, color = type)) + 
    geom_line(size = 1.3) +
    scale_color_manual(values = c("red", "black")) +
    theme(panel.background = element_blank())
  
  p + geom_ribbon(data = forecast_df, aes(x = future_dates, ymin = lwr, ymax = upr), inherit.aes = F,
                  fill = "orange", alpha = 0.3) + 
    geom_line(data = forecast_df, aes(x = future_dates, y = mean), inherit.aes = F, color = "red",
              lwd = 1.3)
}



plot_diagnostics <- function(model){
  p1 <- autoplot(residuals(model)) + 
    geom_abline(slope = 0, intercept = 0, col = "red") + 
    theme(panel.background = element_blank())
  p2 <- ggAcf(residuals(model)) + 
    theme(panel.background = element_blank())
  p3 <- ggPacf(residuals(model)) + 
    theme(panel.background = element_blank())
  grid.arrange(p1, p2, p3, 
               layout_matrix = rbind(c(1, 1),
                                     c(2, 3)))
}



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
    box(width = 5,
        plotOutput("timeseries")
    ),
    box(width = 5,
        plotOutput("demo_breakdown"))
  ),
  
  fluidRow(
    box(title = "ARIMA Model-Fitting",
        width = 2,
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
    ),
    box(width = 5,
        plotOutput("diag"))
  ),
  
  fluidRow(
    imageOutput("image1")
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
    state$trend_data <- get_numbers(va_covid, input$date[1], input$date[2], input$district, input$demo, input$stat)
    state$arima_data <- state$trend_data %>%   
      group_by(date) %>% 
      summarize(sum(count)) %>% 
      mutate(`sum(count)` = c(0, diff(`sum(count)`)))
    state$model_fit  <- Arima(state$arima_data %>% .$"sum(count)", order = c(input$p, input$d, input$q))
  })
  
  output$timeseries <- renderPlot({
    plot_timeseries(state$trend_data)
  })
  
  output$demo_breakdown <- renderPlot({
    plot_demographics(state$trend_data, input$district)
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
