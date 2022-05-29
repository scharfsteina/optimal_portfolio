library(qqplotr)
library(rvest)
library(tidyverse)
library(lubridate)
library(quantmod)
library(ggthemes)
library(patchwork)
library(reshape2)

source("heatmaps.R")
source("interval.R")
source("markowitz.R")
source("qq.R")
source("data_wrangling.R")
source("display_returns.R")
source("linear_combos.R")
source("markowitz_bullet.R")
source("probability_bullet.R")

draw_markowitz(0.1, 0.01, 0.2, 0.05)
ggsave("figures/markowitz_bullet.png", width = 10, height = 6)
draw_interval(0.1, 0.01, 0.2, 0.05, 0.02)
ggsave("figures/probability_bullet.png", width = 10, height = 6)

historical_data <- get_data(.from = make_date(2020,1,1), .to = make_date(2021,12,31))
current_data <- get_data(.from = make_date(2022,1,1), .to = make_date(2022,5,27))

qq_plot(historical_data)
ggsave("figures/qqplot.png", width = 10, height = 6)
qq_plot_ind(historical_data, "DOW") # plot an individual qqplot

cor_heatmap(historical_data)
ggsave("figures/cor_heatmap.png", width = 10, height = 6)
pcor_heatmap(historical_data)
ggsave("figures/pcor_heatmap.png", width = 10, height = 6)

weights_m <- get_weights_markowitz(historical_data[-1], 0.005)
weights_i <- get_weights_interval(historical_data[-1], -0.05, 1)

returns <- get_linear_combos(current_data, weights_m, weights_i)

plot_cumulative(returns)
