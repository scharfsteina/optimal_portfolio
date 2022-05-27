## Plot the daily and cumulative returns using the returns calculated in linear_combos.R

# Plot daily returns
plot_daily <- function(final) {
  final %>%
    pivot_longer(values_to = "returns", names_to = "method", cols = c(markowitz, interval, equal)) %>%
    ggplot(aes(x = date, y = returns, color = method)) +
    geom_line() +
    geom_smooth() +
    expand_limits(y = c(-1, 1)) +
    scale_color_manual(labels = c("Markowitz", "Interval", "Equal Weights"), 
                       values = c("#eb5a0f", "#1c3678", "#249e8f")) +
    labs(title = "Comparing Returns of Portfolio Allocation Strategies on Dow 30",
         x = "Date",
         y = "Return (in %)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "bottom")
}


# Plot cumulative returns
plot_cumulative <- function(final) {
  min_return <- returns %>% 
    select(cumulative_equal, cumulative_markowitz, cumulative_interval) %>%
    min()
  max_return <- returns %>% 
    select(cumulative_equal, cumulative_markowitz, cumulative_interval) %>%
    max()
  
  final %>%
    pivot_longer(values_to = "returns", names_to = "method", cols = c(cumulative_markowitz,
                                                                      cumulative_interval,
                                                                      cumulative_equal)) %>%
    ggplot(aes(x = date, y = returns, color = method)) +
    geom_line() +
    geom_smooth(size = 0, span = 0.6, alpha = 0.1) +
    stat_smooth(geom = "line", size = 0.4) +
    expand_limits(y = c((min_return- 0.1), (max_return + 0.1))) +
    scale_color_manual(labels = c("Markowitz", "Interval", "Equal Weights"), 
                       values = c("#eb5a0f", "#1c3678", "#249e8f")) +
    labs(title = "Comparing Returns of Portfolio Allocation Strategies on Dow 30",
         x = "Date",
         y = "Return (in %)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "bottom")
}
