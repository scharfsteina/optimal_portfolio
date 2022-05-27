library(tidyverse)
library(ggthemes)
library(patchwork)

draw_markowitz <- function(mean_1, var_1, mean_2, var_2, rho = 0, range = 0.4){
  alphas <- seq(0, 1, 0.001)
  
  mean_total <- mean_1 * alphas + mean_2 * (1 - alphas)
  variance <- var_1 * alphas^2 + 2 * alphas * (1 - alphas) * rho * 
    sqrt(var_1 * var_2) + var_2 * (1 - alphas)^2
  variance_rho_p <- var_1 * alphas^2 + 2 * alphas * (1 - alphas) * (rho + range) * 
    sqrt(var_1 * var_2) + var_2 * (1 - alphas)^2
  variance_rho_m <- var_1 * alphas^2 + 2 * alphas * (1 - alphas) * (rho - range) * 
    sqrt(var_1 * var_2) + var_2 * (1 - alphas)^2
  
  df <- tibble(
    mean = mean_total,
    variance = variance,
    var_p = variance_rho_p,
    var_m = variance_rho_m
  ) %>% 
    pivot_longer(cols = 2:ncol(.), names_to = "rhos", values_to = "variance")
  
  plot <- ggplot(df) +
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

draw_markowitz(0.1, 0.01, 0.2, 0.05)

pairwise_markowitz <- function(data){
  symbols <- names(data)
  cor_matrix <- cor(data)

  column_list <- list()
  for(i in 1:ncol(data)){
    column <- list()
    for(j in 1:ncol(data)){
      column1 <- data %>% select(i) %>% pull()
      column2 <- data %>% select(j) %>% pull()
      plot <- draw_markowitz(mean(column1), cor_matrix[i, i], 
                     mean(column2), cor_matrix[j, j], cor_matrix[i, j])
      column[[j]] <- plot
    }
    column_list[[i]] <- column
  }
  
  output <- column_list[[1]][[1]]
  for (i in 1:length(column)){
    temp <- column_list[[i]][[1]]
    for (j in 1:length(column)){
      if(j == 1){
        next
      }
      temp <- temp | column_list[[i]][[j]]
    }
    output <- output / temp
  }
}

# Get optimal weights from markowitz method
opt_markowitz <- function(df, returns){
  mu <- colMeans(df)
  omega <- var(df)
  
  inv_omega <- solve(omega)
  A <- (t(mu) %*% inv_omega %*% rep(1, length(mu)))[1,1]
  B <- (t(mu) %*% inv_omega %*% mu)[1,1]
  C <- (t(rep(1, length(mu))) %*% inv_omega %*% rep(1, length(mu)))[1,1]
  D <- (B * C - A^2)
  a <- ((returns * C - A) / D)
  
  w <- inv_omega %*% (a * mu + ((1 - a * A) / C) * rep(1, length(mu)))
  return(w)
}
