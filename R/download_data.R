library("tidyverse")

#download cases data
cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

#pivot
cases2 <- cases %>% 
  pivot_longer(matches("^\\d"), names_to = "date", values_to = "cases") %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  group_by(`Province/State`, `Country/Region`)
