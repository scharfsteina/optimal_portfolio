# TEST
library(rvest)
library(tidyverse)
library(lubridate)
library(quantmod)


stocks <- html_table(read_html("https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"))[[2]]
stocks <- stocks$Symbol
#stocks <- stocks[stocks!="UTX"] # this library doesn't have UTX data

get_data <- function(.from, .to) {
  data <- NULL
  get_stock <- function(.stock) {
    df <- loadSymbols(.stock, 
                      from = .from, 
                      to = .to, 
                      env = NULL) %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column("date") %>% 
      as.tibble() %>% 
      mutate(date = as.Date(date)) %>% 
      select(1,5) %>% 
      setNames(c("date","close")) %>% 
      mutate(percentage = (close-lag(close))/lag(close)) %>% 
      filter(!is.na(percentage)) %>% 
      select(date, percentage)
  }
  
  for (i in seq_along(stocks)) {
    if (i == 1) {
      data <- get_stock(stocks[i])
    } else {
      temp <- get_stock(stocks[i])$percentage
      data <- cbind(data,temp)
    }
  }
  
  names(data) <- c("date",stocks)
  data <- data %>%
    as.tibble()
}

historical_data <- get_data(.from = make_date(2020,1,1), .to = make_date(2021,12,31))


