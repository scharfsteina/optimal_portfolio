source("data-wrangling.R")
source("display-results.R")
source("markowitz.R")


opt_weights <- opt_markowitz(historical_data %>% select(-1), 0.005)

markowitz_returns <- get_linear_combos(current_data, opt_weights)

plot_cumulative(markowitz_returns)
