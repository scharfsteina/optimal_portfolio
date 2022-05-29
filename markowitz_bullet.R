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
    var_m = variance_rho_m) %>% 
    pivot_longer(cols = 2:ncol(.), names_to = "rhos", values_to = "variance")
  
  mins <- data %>% 
    group_by(rhos) %>% 
    summarise(variance = min(variance)) %>% 
    left_join(data, by = c("rhos","variance"))
   
  
  plot <- ggplot(data) +
    geom_path(aes(variance, mean, linetype = rhos)) +
    geom_point(data = mins,
               mapping = aes(variance,
                             mean))+
    labs(
      title = "Markowitz bullet",
      x = "Variance/Volatility",
      y = "Expected Return",
      linetype = NULL
    ) +
    scale_linetype_manual(labels = c(str_c("\U0003C1 = ", rho - range),
                                     str_c("\U0003C1 = ", rho + range),
                                     str_c("\U0003C1 = ", rho)),
                          values = c(2, 3, 1)) +
    theme_few() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.1, .85),
          legend.text=element_text(size=15),
          legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
          legend.title = NULL)
  
  return(plot)
}

