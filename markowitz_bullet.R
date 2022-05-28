## Draw a theoretical Markowitz bullet based on a portfolio with two stocks

draw_markowitz <- function(mean_1, var_1, mean_2, var_2, rho = 0, range = 0.4) {
  alphas <- seq(0, 1, 0.001)
  
  mean_total <- mean_1 * alphas + mean_2 * (1 - alphas)
  variance <- var_1 * alphas^2 + 2 * alphas * (1 - alphas) * rho * 
    sqrt(var_1 * var_2) + var_2 * (1 - alphas)^2
  variance_rho_p <- var_1 * alphas^2 + 2 * alphas * (1 - alphas) * (rho + range) * 
    sqrt(var_1 * var_2) + var_2 * (1 - alphas)^2
  variance_rho_m <- var_1 * alphas^2 + 2 * alphas * (1 - alphas) * (rho - range) * 
    sqrt(var_1 * var_2) + var_2 * (1 - alphas)^2
  
  data <- tibble(
    mean = mean_total,
    variance = variance,
    var_p = variance_rho_p,
    var_m = variance_rho_m
  ) %>% 
    pivot_longer(cols = 2:ncol(.), names_to = "rhos", values_to = "variance")
  
  plot <- ggplot(data) +
    geom_path(aes(variance, mean, linetype = rhos)) +
    labs(
      title = paste0("Markowitz bullet"),
      x = "Variance/Volatility",
      y = "Mean",
      linetype = NULL
    ) +
    scale_linetype_manual(labels = c(str_c("rho = ", rho - range),
                                     str_c("rho = ", rho + range),
                                     str_c("rho = ", rho)),
                          values = c(2, 3, 1)) +
    theme_few() +
    theme(legend.position = c(0.75, 0.25))
  
  return(plot)
}
