library(tidyverse)
library(janitor)
library(lubridate)

# Load data locally (need to change to API call)

va_bysex <- read_csv("data/VDH-COVID-19-PublicUseDataset-Cases_By-Sex.csv")
va_byrace <- read_csv("data/VDH-COVID-19-PublicUseDataset-Cases_By-Race-Ethnicity.csv")
va_pop <- read_csv("data/VDH-Population_By_Health_District.csv")


# Clean datasets
va_bysex_clean <- janitor::clean_names(va_bysex) %>% 
  filter(!is.na(health_district), sex != "Not Reported") %>% 
  mutate(demographic = "sex") %>% 
  rename(level = sex)

va_byrace_clean <- janitor::clean_names(va_byrace) %>% 
  filter(!is.na(health_district_or_health_district_group), race_and_ethnicity != "Not Reported") %>% 
  mutate(demographic = "race") %>% 
  rename(level = race_and_ethnicity, health_district = health_district_or_health_district_group)


# Keep only data where dates overlap
date_ranges <- intersect(va_bysex_clean$report_date, va_byrace_clean$report_date)


# Merge sex and race datasets
va_merged <- rbind(va_bysex_clean, va_byrace_clean) %>% 
  filter(report_date %in% date_ranges) %>% 
  mutate(date = lubridate::mdy(report_date)) %>% 
  gather(key = "statistic", value = "count", number_of_cases, number_of_hospitalizations, number_of_deaths) %>% 
  mutate(statistic = statistic %>% factor(levels = c("number_of_cases", "number_of_hospitalizations",
                                           "number_of_deaths"),
                                labels = c("cases", "hospitalizations", "deaths")))


va_complete <- inner_join(va_merged, va_pop)


# Save dataset to be loaded in shiny app
saveRDS(va_complete, "data/va_covid")
