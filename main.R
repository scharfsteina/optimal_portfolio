library(ggpubr)
library(qqplotr)
library(rvest)
library(tidyverse)
library(lubridate)
library(quantmod)
library(ggthemes)
library(patchwork)


draw_markowitz(0.1, 0.01, 0.2, 0.05)

historical_data <- get_data(.from = make_date(2020,1,1), .to = make_date(2021,12,31))
current_data <- get_data(.from = make_date(2022,1,1), .to = make_date(2022,5,1))

qq_plot(historical_data[-1])
qq_plot(current_data[-1])

weights_markowitz <- get_weights_markowitz(historical_data %>% select(-1), 0.05)
weights_interval <- get_weights_interval(historical_data %>% select(-1), -0.05, 0.5)

returns <- get_linear_combos(current_data, weights_markowitz, weights_interval)

plot_cumulative(returns)
