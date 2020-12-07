library(tidyverse)

va_covid <- read_rds("va_covid")

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


plot_demographics <- function(df, district, covid_data){
  
  df_summarized <- df %>% 
    group_by(level) %>% 
    summarize(count = count %>% diff())
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
    scale_color_manual(values = c("orange", "red")) + 
    theme(legend.position = "none")
    
  print(p, vp = viewport(angle = -15))
  
}


plot_arima <- function(df, model, end_date){
  
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
    scale_color_manual(labels = c("fitted", "data"),
                       values = c("red", "black")) +
    theme(panel.background = element_blank()) + 
    ylim(0, NA)
  
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


# Create moving bar-plots

#va_totals <- va_covid %>% 
#  filter(demographic == "sex") %>% 
#  group_by(health_district, date, statistic) %>% 
#  summarize(total_count = sum(count)) %>% 
#  group_by(date) %>% 
#  mutate(rank = rank(-total_count),
#         Value_lbl = paste0(" ", round(total_count))) %>% 
#  group_by(health_district) %>% 
#  filter(rank <=10) %>%
#  ungroup() %>% 
#  mutate(health_district = as.factor(health_district))


#static_plot <- va_totals %>% 
#  ggplot(aes(rank, group = health_district, 
#             fill = health_district, color = health_district)) +
#  geom_tile(aes(y = total_count/2,
#                height = total_count,
#                width = 0.9), alpha = 0.8, color = NA) + 
#  geom_text(aes(y = 0, label = paste(health_district, " ")), vjust = 0.2, hjust = 1) +
#  geom_text(aes(y = total_count, label = Value_lbl, hjust = 0)) +
#  coord_flip(clip = "off", expand = FALSE) +
#  scale_y_continuous(labels = scales::comma) +
#  scale_x_reverse() +
#  guides(color = FALSE, fill = FALSE) +
#  theme(axis.line       = element_blank(),
#        axis.text.x     = element_blank(),
#        axis.text.y     = element_blank(),
#        axis.ticks      = element_blank(),
#        axis.title.x    = element_blank(),
#        axis.title.y    = element_blank(),
#        legend.position = "none",
#        panel.background = element_blank(),
#        panel.border = element_blank(),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.grid.major.x = element_line(size = 0.1, color = "grey" ),
#        panel.grid.minor.x = element_line(size = 0.1, color = "grey" ),
#        plot.title = element_text(size = 25, hjust = 0.5, face = "bold", colour = "grey", vjust = -1),
#        plot.subtitle = element_text(size = 18, hjust = 0.5, face="italic", color = "grey"),
#        plot.caption = element_text(size = 8, hjust = 0.5, face = "italic", color = "grey"),
#        plot.background=element_blank(),
#        plot.margin = margin(2,2, 2, 4, "cm"))

#anim <- static_plot + 
#  transition_states(date, transition_length = 1, state_length = 2) + 
#  view_follow(fixed_x = TRUE) + 
#  labs(title = 'COVID-19 cases over Summer 2020 : {closest_state}',  
#       subtitle  =  "Ten countries with highest case numbers")

#animate(anim, duration = 30, fps = 20,  width = 1200, height = 1000, 
#        start_pause = 20,
#        end_pause = 20,
#        renderer = av_renderer("gganim.av"))


