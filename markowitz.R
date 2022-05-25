library(tidyverse)
library(ggthemes)
library(patchwork)

draw_markowitz <- function(mean_1, var_1, mean_2, var_2, rho = 0){
  alphas <- seq(0, 1, 0.001)
  
  mean_total <- mean_1 * alphas + mean_2 * (1 - alphas)
  variance <- var_1 * alphas^2 + 2 * alphas * (1 - alphas) * rho * 
    sqrt(var_1 + var_2) + var_2 * (1 - alphas)^2
  
  df <- tibble(
    mean = mean_total,
    variance = variance
  )
  
  min_alpha <- alphas[min(variance) == variance]
  
  plot <- ggplot(df, aes(variance, mean)) +
    geom_path() +
    labs(
      title = paste0("Markowitz bullet with mu1 = ", mean_1, 
                     ", var1 = ", var_1, ", mu2 = ", mean_2, ", var2 = ", var_1),
      x = "Variance/Volatility",
      y = "Mean"
    ) +
    theme_few()
  
  return(plot)
}

#draw_markowitz(0.1, 0.01, 0.2, 0.05)

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

#pairwise_markowitz(data_cleaned %>% select(-1))

min_markowitz <- function(mu, omega, returns){
  inv_omega <- solve(omega)
  A <- (t(mu) %*% inv_omega %*% rep(1, length(mu)))[1,1]
  B <- (t(mu) %*% inv_omega %*% mu)[1,1]
  C <- (t(rep(1, length(mu))) %*% inv_omega %*% rep(1, length(mu)))[1,1]
  D <- (B * C - A^2)
  a <- ((returns * C - A) / D)
  
  w <- inv_omega %*% (a * mu + ((1 - a * A) / C) * rep(1, length(mu)))
  return(w)
}