## Plot qq-plots for the daily returns of the stocks

qq_plot <- function(data) {
  data %>%
    pivot_longer(names_to = "symbol", values_to = "returns", cols = AXP:VZ) %>%
    ggplot(aes(sample = returns)) +
    stat_qq_band(color = "green", alpha = 0.2) +
    stat_qq_point(color = "grey10", size = 0.5, alpha = 0.75) +
    stat_qq_line(color = "red", size = 0.25) +
    facet_wrap(~ symbol) +
    theme_classic() +
    labs(x = "Theoretical Quantiles",
         y = "Empilircal Quantiles")
}