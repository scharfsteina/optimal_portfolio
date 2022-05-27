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

opt_weights <- opt_markowitz(historical_data %>% select(-1), 0.005)

markowitz_returns <- get_linear_combos(current_data, opt_weights)

plot_cumulative(markowitz_returns)