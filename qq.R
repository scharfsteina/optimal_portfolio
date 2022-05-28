## Plot qq-plots for the daily returns of the stocks

qq_plot <- function(data) {
  data %>%
    pivot_longer(names_to = "symbol", values_to = "returns", cols = MMM:WMT) %>%
    ggplot(aes(sample = returns)) +
    stat_qq_band(color = "green", alpha = 0.2) +
    stat_qq_point(color = "grey10", size = 0.3, alpha = 0.75) +
    stat_qq_line(color = "red", size = 0.25) +
    facet_wrap(~ symbol) +
    theme_few() +
    labs(x = "Theoretical Quantiles",
         y = "Empilircal Quantiles")
}

qq_plot_ind <- function(data, stock) {
  data %>% 
    select(date, stock) %>% 
    ggplot(aes(sample = get(stock))) +
    stat_qq_band(color = "green", alpha = 0.2, size = .5) +
    stat_qq_line(color = "red", size = 1) +
    stat_qq_point(color = "grey10", size = 1, alpha = 0.75) +
    theme_few() +
    labs(x = "Theoretical Quantiles",
         y = "Empilircal Quantiles",
         title = paste(stock, "QQPlot"))
}
