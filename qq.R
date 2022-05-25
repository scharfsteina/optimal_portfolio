library(ggpubr)
library(qqplotr)

qq <- function(data) {
  n_stocks <- length(data)
  n_days <- nrow(data)
  quantiles <- qnorm((1:n_days) / n_days)
  par(mfrow = c(5, 5), mar = c(3, 1, 1, 1), mgp = c(2, 1, 0), cex.axis = 0.75, pty = "s")

  for (i in 2:n_stocks) {
    stock <- sort(data %>% pull(i))
    stock_norm <- (stock - mean(stock)) / sd(stock)

    plot(quantiles, stock_norm, xlab = "Theoretical Normal Quantile",
         ylab = "Empirical Quantile", main = NA)

    segments(-5, -5, 5, 5, col = 2)

    upper_Bounds <- qnorm(qbeta(0.5 + 0.95/2, shape1 = 1:n_days, shape2 = n_days - 1:n_days + 1))
    lower_Bounds <- qnorm(qbeta(0.5 - 0.95/2, shape1 = 1:n_days, shape2 = n_days - 1:n_days + 1))

    lines(quantiles, upper_Bounds, type = "s", col = 3)
    lines(quantiles, lower_Bounds, type = "s", col = 3)
  }
}

qq_gg <- function(data) {
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