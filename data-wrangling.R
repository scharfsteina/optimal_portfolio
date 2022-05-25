# TEST
library(rvest)
library(tidyverse)
library(lubridate)
library(quantmod)

stocks <- html_table(read_html("https://www.dogsofthedow.com/dowcomp.htm"))[[1]]
stocks <- stocks %>%
  pull(Symbol)
  
# this is only 2022 data
# ex <- html_table(read_html(paste0("https://finance.yahoo.com/quote/",stocks[1],"/history")))[[1]]
# ndays <- lengths(ex)[1]
# data <- data.frame(matrix(nrow = ndays, ncol = 1))
# names(data) <- "date"

get_stock <- function(.stock) {
  df <- loadSymbols(.stock, from = as.Date("2020-01-01"), to = as.Date("2021-12-31"))
  print(df)
}


get_returns <- function(df) {
  df <- df %>%
    mutate(date = as.Date(Date, format = "%b %d, %Y"),
           close = `Close*` %>% as.double()) %>%
    filter(!is.na(close)) %>%
    arrange(date) %>%
    mutate(percentage = (close-lag(close))/lag(close)) %>% 
    filter(!is.na(percentage)) %>% 
    select(date, percentage)
}


for (i in seq_along(stocks)) {
  html <- read_html(paste0("https://finance.yahoo.com/quote/",stocks[i],"/history"))
  data <- html_table(html)[[1]] %>% 
    get_returns() %>% 
    left_join(data, by = "date")
}

names(data) <- c("date",stocks)
data_cleaned <- data %>%
  na.omit()
