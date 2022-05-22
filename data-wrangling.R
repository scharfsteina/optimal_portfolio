# TEST
library(rvest)
library(tidyverse)
library(lubridate)

stocks <- html_table(read_html("https://www.dogsofthedow.com/dowcomp.htm"))[[1]]
stocks <- stocks %>%
  pull(Symbol)
  
# this is only 2022 data
ex <- html_table(read_html(paste0("https://finance.yahoo.com/quote/",stocks[1],"/history")))[[1]]
ndays <- lengths(ex)[1]
data <- data.frame(matrix(nrow = ndays, ncol = 1))
names(data) <- "date"


for (i in seq_along(stocks)) {
  html <- read_html(paste0("https://finance.yahoo.com/quote/",stocks[i],"/history"))
  data <- html_table(html)[[1]] %>% 
    get_returns() %>% 
    left_join(data, by = "date")
}
names(data) <- c("date",stocks)
data_cleaned <- data %>%
  na.omit()

get_returns <- function(df) {
  df %>%
    mutate(date = as.Date(Date, format = "%b %d, %Y"),
           close = `Close*` %>% as.double()) %>%
    filter(!is.na(close)) %>%
    arrange(date) %>%
    mutate(percentage = 100*(close-lag(close))/lag(close)) %>% 
    filter(!is.na(percentage)) %>% 
    select(date, percentage)
}

