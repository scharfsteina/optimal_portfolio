## Draw a theoretical Interval model bullet based on a portfolio with two stocks with maximum return = +inf

draw_interval <- function(mean_1, var_1, mean_2, var_2, rho = 0) {
  alphas <- seq(0, 1, 0.001)
  
  mean_total = alphas * mean_1 + (1 - alphas) * mean_2
  
  lowest_return = pnorm((rho - mean_total) / sqrt(alphas^2 * var_1 + (1 - alphas)^2 * var_2))
  
  data <- tibble(
    mean = mean_total,
    lowest_return = lowest_return
  )
  
  data %>%
    ggplot() +
    geom_path(aes(lowest_return, mean)) +
    labs(
      title = paste0("Probability Bullet"),
      x = paste("Probability of a Return < ", rho, sep = ""),
      y = "Expected Return"
    ) +
    theme_few() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.75, 0.25))
}

