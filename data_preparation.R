library(tidyverse)
library(janitor)
library(lubridate)

#va_bydistrict <- read_csv("/cloud/project/ReproducibilityRaptors/data/VDH-COVID-19-PublicUseDataset-Cases_By-District-Death-Hospitalization.csv")
va_bysex <- read_csv("/cloud/project/ReproducibilityRaptors/data/VDH-COVID-19-PublicUseDataset-Cases_By-Sex.csv")
va_byrace <- read_csv("/cloud/project/ReproducibilityRaptors/data/VDH-COVID-19-PublicUseDataset-Cases_By-Race-Ethnicity.csv")


va_bysex_clean <- janitor::clean_names(va_bysex) %>% 
  filter(!is.na(health_district), sex != "Not Reported") %>% 
  mutate(demographic = "sex") %>% 
  rename(level = sex)

va_byrace_clean <- janitor::clean_names(va_byrace) %>% 
  filter(!is.na(health_district_or_health_district_group), race_and_ethnicity != "Not Reported") %>% 
  mutate(demographic = "race") %>% 
  rename(level = race_and_ethnicity, health_district = health_district_or_health_district_group)


date_ranges <- intersect(va_bysex_clean$report_date, va_byrace_clean$report_date)

va_merged <- rbind(va_bysex_clean, va_byrace_clean) %>% 
  filter(report_date %in% date_ranges) %>% 
  rename(date = report_date, 
         cum_number_of_cases = number_of_cases, 
         cum_number_of_hospitalizations = number_of_hospitalizations,
         cum_number_of_deaths = number_of_deaths) %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  group_by(health_district, demographic, level) %>% 
  mutate(number_of_cases = c(0, diff(cum_number_of_cases)),
         number_of_hospitalizations = c(0, diff(cum_number_of_hospitalizations)),
         number_of_deaths = c(0, diff(cum_number_of_deaths))) %>% 
  ungroup() %>% 
  gather(statistic, count, number_of_cases, number_of_hospitalizations, number_of_deaths) %>% 
  mutate(statistic = statistic %>% factor(levels = c("number_of_cases", "number_of_hospitalizations",
                                           "number_of_deaths"),
                                labels = c("cases", "hospitalizations", "deaths")))

saveRDS(va_merged, "/cloud/project/ReproducibilityRaptors/data/va_covid")

