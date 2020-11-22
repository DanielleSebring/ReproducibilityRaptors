library(tidyverse)


# Create moving bar-plots

va_totals <- va_covid %>% 
  filter(demographic == "sex") %>% 
  group_by(health_district, date, statistic) %>% 
  summarize(total_count = sum(count)) %>% 
  group_by(date) %>% 
  mutate(rank = rank(-total_count),
         Value_lbl = paste0(" ", round(total_count))) %>% 
  group_by(health_district) %>% 
  filter(rank <=10) %>%
  ungroup() %>% 
  mutate(health_district = as.factor(health_district))


static_plot <- va_totals %>% 
  ggplot(aes(rank, group = health_district, 
             fill = health_district, color = health_district)) +
  geom_tile(aes(y = total_count/2,
                height = total_count,
                width = 0.9), alpha = 0.8, color = NA) + 
  geom_text(aes(y = 0, label = paste(health_district, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = total_count, label = Value_lbl, hjust = 0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line       = element_blank(),
        axis.text.x     = element_blank(),
        axis.text.y     = element_blank(),
        axis.ticks      = element_blank(),
        axis.title.x    = element_blank(),
        axis.title.y    = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, color = "grey" ),
        panel.grid.minor.x = element_line(size = 0.1, color = "grey" ),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold", colour = "grey", vjust = -1),
        plot.subtitle = element_text(size = 18, hjust = 0.5, face="italic", color = "grey"),
        plot.caption = element_text(size = 8, hjust = 0.5, face = "italic", color = "grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim <- static_plot + 
  transition_states(date, transition_length = 1, state_length = 2) + 
  view_follow(fixed_x = TRUE) + 
  labs(title = 'COVID-19 cases over Summer 2020 : {closest_state}',  
       subtitle  =  "Ten countries with highest case numbers")

animate(anim, duration = 30, fps = 20,  width = 1200, height = 1000, 
        start_pause = 20,
        end_pause = 20,
        renderer = av_renderer("gganim.av"))


