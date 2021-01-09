# library(COVID19) # no data for level = 2 Russia :(
library(tidyverse)
library(rio)
library(lubridate)

covid_at_date = function(date = "2020-12-19") {
  prefix = "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/"
  url = paste0(prefix, lubridate::ymd(date) %>% format("%m-%d-%Y"), ".csv")
  daily = rio::import(url)
  daily$date = lubridate::ymd(date)
  return(daily)
}

x = covid_at_date("2020-12-19")

covid_at_dates = function(from = "2020-12-19", to = "2020-12-30") {
  from_char = as.character(from)
  to_char = as.character(to)
  from_date = lubridate::ymd(from)
  to_date = lubridate::ymd(to)

  dates = seq(from_date, to_date, by = "1 day")
  covid_data = NULL

  for (i in 1:length(dates)) {
    date_char = as.character(dates[i])
    message("Downloading data for ", date_char)
    daily_data = covid_at_date(date_char)
    # daily_data = NULL
    covid_data = dplyr::bind_rows(covid_data, daily_data)
  }
  return(covid_data)
}

x = covid_at_dates(from = "2020-11-01", to = "2021-01-05")


moscov = filter(x, Province_State == "Moscow")
moscov


seventy = tail(diff(moscov$Deaths), -1)
qplot(diff(seventy), binwidth=1)
(6/7)^59


qov = filter(x, Province_State == "Quebec")
qplot(diff(qov$Confirmed), binwidth=1)


irkcov = filter(x, Province_State == "Irkutsk Oblast")
seventy_ir = tail(diff(irkcov$Confirmed), -1)
qplot(diff(seventy_ir), binwidth=1)
qplot(diff(tail(seventy_ir, 29))) + geom_histogram(binwidth = 1, col = "white")

ruscov = filter(x, Country_Region == "Russia")

ruscov$Province_State %>% unique()


spbcov = filter(x, Province_State == "Saint Petersburg")
seventy_spb = tail(diff(spbcov$Deaths), -1)
qplot(diff(tail(seventy_spb, 29)), binwidth=1)

qplot(diff(seventy_spb)) + geom_histogram(binwidth = 1, col = "white")
