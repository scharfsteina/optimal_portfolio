## Draw a theoretical Interval model bullet based on a portfolio with two stocks with maximum return = +inf

draw_interval <- function(mean_1, var_1, mean_2, var_2, rho = 0) {
  alphas <- seq(0, 1, 0.001)
  
  mean_total <- alphas * mean_1 + (1 - alphas) * mean_2
  lowest_return <- pnorm((rho - mean_total) / sqrt(alphas^2 * var_1 + (1 - alphas)^2 * var_2))
  
  mean_opt <- mean_total[which.min(lowest_return)]
  lowest_return_opt <- min(lowest_return)
  mean_zero <- mean_total[1]
  lowest_return_zero <- lowest_return[1]
  mean_one <- mean_total[1001]
  lowest_return_one <- lowest_return[1001]

  
  data <- tibble(
    mean = mean_total,
    lowest_return = lowest_return
  )
  
  data_opt <- tibble(
    mean_opt = c(mean_opt, mean_zero, mean_one),
    lowest_return_opt = c(lowest_return_opt, lowest_return_zero, lowest_return_one)
  )
  
  data %>%
    ggplot() +
    geom_path(aes(lowest_return, mean)) +
    geom_point(data = data_opt, aes(x = lowest_return_opt, y = mean_opt), inherit.aes = FALSE) +
    labs(
      title = paste0("Probability Bullet"),
      x = paste("Probability of a Return < ", rho, sep = ""),
      y = "Expected Return"
    ) +
    theme_few() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.75, 0.25))
}

