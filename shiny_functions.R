library(tidyverse)
library(rjson)
library(coronavirus)
library(httr)
library(jsonlite)


url <- "https://liproduction-reportsbucket-bhk8fnhv1s76.s3-us-west-1.amazonaws.com/v1/latest/timeseries-byLocation.json"
res <- GET(url,
           query = list(countryName = "United States",
                        stateName = "California",
                        countyName = "dofijewoifjeowifjeofijf"))
response <- content(res, as = "text", encoding = "UTF-8")

json_file <- "/cloud/project/ReproducibilityRaptors/data/timeseries_sample.json"
json_data <- rjson::fromJSON(paste(readLines(json_file), collapse = ""))

timeseries_df <- data_frame(
  country = map_chr(json_data, "countryName"),
  coordinates = map(json_data, "coordinates"),
  population = map_dbl(json_data, "population"),
  dates = map(json_data, "dates"))


timeseries_df <- timeseries_df %>% 
  mutate(latitude = sapply(coordinates, function(x){x[[1]]}),
         longitude = sapply(coordinates, function(x){x[[2]]})) %>% 
  select(-coordinates) %>% 
  tidyr::unnest(dates) %>% 
  mutate(date = names(dates) %>% as.Date(),
         cases = map_dbl(dates, "cases"),
         deaths = map_dbl(dates, "deaths"),
         recovered = map_dbl(dates, "recovered")) %>% 
  gather(type, cases, cases, deaths, recovered) %>% 
  select(-dates)




#timeseries <- read.csv("/cloud/project/ReproducibilityRaptors/data/timeseries-jhu.csv")


get_numbers <- function(df, location, variable){
  return(df %>% 
           filter(country == location, type == variable) %>% 
           select(date, cases)
  )
}


plot_timeseries <- function(df, location, variable){
  df_filtered <- get_numbers(df, location, variable)
  df_filtered %>% 
    ggplot(aes(x = date, y = cases)) + 
    geom_line() + 
    labs(title = location)
}


plot_timeseries(timeseries_df, "Andorra", "deaths")


hospitals <- read.csv("/cloud/project/ReproducibilityRaptors/data/hospitals.csv")

url <- "https://api.theuscovidatlas.org/v1/data"
res <- GET(url, query = list(state = "va",
                             start = 20200122,
                             end = 20200123,
                             category = "data"))
json_data <- rjson::fromJSON(file = res$url)$data

timeseries_df <- json_data %>% 
  map_df(function(x){as_data_frame(x)}) %>% 
  rename(county = `County Name`) %>% 
  select(-GEOID)


timeseries_df %>% gather(date, cases, -county)
