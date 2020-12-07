require(tidyverse)
require(gt)

" 
This script contains all of the functions used by the Shiny app.
Functions are designed to either manipulate data or output plots/tables.

"

# Filter and return COVID data based on user-specified parameters

get_numbers <- function(df, start, end, hd, demo, stat){
  
  df_filtered <- df %>%
    filter(date >= start, date <= end, 
           health_district == hd,
           demographic == demo,
           statistic == stat)
  return(df_filtered)
  
}

# Create time-series plot

plot_timeseries <- function(df){
  
  df %>% 
    ggplot(aes(x = date, y = count, fill = level)) + 
    geom_area(alpha = 0.8) +
    theme(panel.background = element_blank()) + 
    scale_fill_brewer(palette = "Dark2")
  
}

# Create and plot summary table and map visual of VA health districts

plot_demographics <- function(covid_data, district){
  
  va_map_dat <- covid_data %>% 
    group_by(health_district) %>% 
    summarize(lon = mean(longitude),
              lat = mean(latitude),
              pop = mean(total_population)) %>%
    mutate(is_district = health_district == district) %>% 
    relocate(health_district, .after = last_col()) %>% 
    usmap_transform()
  p <- plot_usmap("counties", include = c("VA")) + 
    geom_point(data = va_map_dat, aes(x = lon.1, y = lat.1, size = pop, color = is_district), 
               alpha = 1) +
    scale_color_manual(values = c("#E87722", "#4169e1")) + 
    theme(legend.position = "none")
    
  print(p, vp = viewport(angle = -15))
  
}

# Fit ARIMA model based on user-specificed parameters

fit_arima <- function(arima_data, p, d, q, sp, sd, sq, period){
  
  arima_fit <- try(
    Arima(arima_data$"sum(count)", 
          order = c(p, d, q), 
          seasonal = list(order = c(sp, sd, sq), period = period))
  )
  
  return(arima_fit)
  
}

# Plot ARIMA model fitting and forecasting

plot_arima <- function(df, model, end_date, p, d, q, sp, sd, sq, period){
  
  future_dates <- seq(end_date, end_date + 13, by = "day")
  model_forecast <- predict(model, n.ahead = 14)
  forecast_df <- data_frame(mean = model_forecast$pred) %>% 
    mutate(lwr = mean - 1.96*model_forecast$se,
           lwr = ifelse(lwr < 0, 0, lwr),
           upr = mean + 1.96*model_forecast$se,
           future_dates = future_dates)
  
  fitted_df <- df %>% 
    mutate(fitted_vals = fitted(model)) %>% 
    gather(type, value, `sum(count)`, fitted_vals)
  
  p <- ggplot(data = fitted_df, aes(x = date, y = value, color = type)) + 
    geom_line(size = 1.3) +
    ylim(0, NA) + 
    scale_color_manual(labels = c("fitted", "data"),
                       values = c("red", "black")) +
    theme(panel.background = element_blank()) + 
    ggtitle(paste0("ARIMA(", p, ", ", d, ", ", q, "), (", sp, ", ", sd, ", ", sq, ")", period)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("count")
  
  p + geom_ribbon(data = forecast_df, aes(x = future_dates, ymin = lwr, ymax = upr), inherit.aes = F,
                  fill = "orange", alpha = 0.3) + 
    geom_line(data = forecast_df, aes(x = future_dates, y = mean), inherit.aes = F, color = "red",
              lwd = 1.3)
  
}


# Function for ARIMA model diagnostic plots (residual plot, ACF and PACF plots)

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


#cumsum takes in a vector x of numeric values and returns a vector y
#which stores the cumulative sum; y[i] is the cumulative value of x[1],x[2]
#,...,x[i]
cumsum <- function(x)
{
  y <- vector(length = length(x))
  for (i in 1:length(y))
  {
    y[i] <- sum(x[1:i])
  }
  return(y)
}

#this function will calculate summary statistics for a specific district
summary_given_district <- function(X, district){
  
  #breakup the dataframe X using the input district and then by type of
  #statistic
  X.district <- subset(X,health_district == district)
  X.district.cases <- subset(X.district,statistic == "cases")
  X.district.hospital <- subset(X.district,statistic == "hospitalizations")
  X.district.deaths <- subset(X.district,statistic == "deaths")
  
  #calculate the mean cases, hospitalizations, deaths
  mean_cases <- mean(X.district.cases$count)
  mean_hospital <- mean(X.district.hospital$count)
  mean_deaths <- mean(X.district.deaths$count)
  
  #calculate the median cases, hospitalizations, deaths 
  median_cases <- median(X.district.cases$count)
  median_hospital <- median(X.district.hospital$count)
  median_deaths <- median(X.district.deaths$count)
  
  #calculate the standard deviation of the cases, hospitalizations, deaths 
  sd_cases <- sd(X.district.cases$count)
  sd_hospital <- sd(X.district.hospital$count)
  sd_deaths <- sd(X.district.deaths$count)
  
  #bind the means,medians, and standard deviations as well as a description
  #column
  means <- rbind(mean_cases, mean_hospital,mean_deaths)
  medians <- rbind(median_cases,median_hospital,median_deaths)
  standard_devs <- rbind(sd_cases,sd_hospital,sd_deaths)
  
  description <- c("Cases", "Hospitalizations","Deaths")
  
  #form the final data frame, give the column names and then convert to a table
  summary <- data.frame(cbind(description,round(means,3),round(medians,3),round(standard_devs,3)))
  names(summary) <- c("Type","Mean","Median","Standard Deviation")
  
  #final_table is the final table object
  final_table <- summary %>% 
    gt() %>% 
    tab_spanner(label = "Statistic", columns = vars("Mean", "Median", "Standard Deviation"))
  
  return(final_table)
  
}


