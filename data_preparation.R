library(tidyverse)
library(janitor)
library(lubridate)
library(RSocrata)

# Load data locally (need to change to API call)

#va_bysex <- read_csv("data/VDH-COVID-19-PublicUseDataset-Cases_By-Sex.csv")
#va_byrace <- read_csv("data/VDH-COVID-19-PublicUseDataset-Cases_By-Race-Ethnicity.csv")

va_bysex  <- read.socrata("https://data.virginia.gov/resource/tdt3-q47w.json")
va_byrace <- read.socrata("https://data.virginia.gov/resource/9sba-m86n.json")
va_byage  <- read.socrata("https://data.virginia.gov/resource/uktn-mwig.json")
va_pop    <- read_csv("VDH-Population_By_Health_District.csv")

clean_data_bysex <- function(dat_sex){
  va_bysex_clean <- janitor::clean_names(dat_sex) %>% 
    filter(!is.na(health_district), sex != "Not Reported") %>% 
    mutate(demographic = "sex") %>% 
    rename(level = sex)
  return(va_bysex_clean)
}

clean_data_byrace <- function(dat_race){
  va_byrace_clean <- janitor::clean_names(dat_race) %>% 
    filter(!is.na(health_district_or_health), race_and_ethnicity != "Not Reported") %>% 
    mutate(demographic = "race") %>% 
    rename(level = race_and_ethnicity, health_district = health_district_or_health)
  return(va_byrace_clean)
}


clean_data_byage <- function(dat_age){
  va_byage_clean <- janitor::clean_names(dat_age) %>% 
    filter(!is.na(health_district), age_group != "Not Reported") %>% 
    mutate(demographic = "age") %>% 
    rename(level = age_group)
  return(va_byage_clean)
}


# Clean and combine datatasets
merge_vadata <- function(dat_sex, dat_race, dat_age, dat_pop){
  
  # Clean column names
  va_bysex_clean  <- clean_data_bysex(dat_sex)
  va_byrace_clean <- clean_data_byrace(dat_race)
  va_byage_clean  <- clean_data_byage(dat_age)
  
  # Keep only data where dates overlap
  date_ranges <- intersect(va_bysex_clean$report_date, va_byrace_clean$report_date) %>% 
    intersect(va_byage_clean$report_date)
  
  # Merge sex, race, and age datasets
  va_merged <- rbind(va_bysex_clean, va_byrace_clean) %>% 
    rbind(va_byage_clean) %>% 
    filter(report_date %in% date_ranges, report_date != "2020-11-01 EDT") %>% 
    mutate(date = lubridate::ymd(report_date)) %>% 
    gather(key = "statistic", value = "count", number_of_cases, number_of_hospitalizations, number_of_deaths) %>% 
    mutate(statistic = statistic %>% factor(levels = c("number_of_cases", "number_of_hospitalizations",
                                                       "number_of_deaths"),
                                            labels = c("cases", "hospitalizations", "deaths")),
           count = as.numeric(count)) %>% 
    select(-report_date)
  
  # Join with population data
  va_complete <- inner_join(va_merged, va_pop, by = "health_district")
  
  return(va_complete)
}


va_complete <- merge_vadata(va_bysex, va_byrace, va_byage)

# Save dataset to be loaded in shiny app
saveRDS(va_complete, "va_covid")



