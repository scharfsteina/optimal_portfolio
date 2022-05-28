# Pairwise Markowitz
# Data passed in assumes to be without the date column

draw_pairwise_markowitz <- function(data){
  symbols <- names(data)
  cor_matrix <- cor(data)
  
  column_list <- list()
  for(i in 1:ncol(data)){
    column <- list()
    for(j in 1:ncol(data)){
      column1 <- data %>% select(i) %>% pull()
      column2 <- data %>% select(j) %>% pull()
      plot <- draw_marko_simple(mean(column1), var(column1), 
                             mean(column2), var(column2), cor_matrix[i, j])
      column[[j]] <- plot
    }
    column_list[[i]] <- column
  }
  
  output <- column_list[[1]][[1]]
  for (i in 1:ncol(data)){
    for (j in 1:ncol(data)){
      if (i == 1 & j == 1){
        next
      }
      output <- output + column_list[[i]][[j]]
    }
  }
  return(output + plot_layout(nrow = ncol(data)) + plot_annotation(
    title = "Pairwise correlation between stocks"
  ))
}



draw_marko_simple <- function(mean_1, var_1, mean_2, var_2, rho = 0){
  if (rho == 1){
    return(ggplot() + labs(x = NULL, y = NULL))
  }
  
  alphas <- seq(0, 1, 0.001)
  
  mean_total <- mean_1 * alphas + mean_2 * (1 - alphas)
  variance <- var_1 * alphas^2 + 2 * alphas * (1 - alphas) * rho * 
    sqrt(var_1 * var_2) + var_2 * (1 - alphas)^2
  
  data <- tibble(
    mean = mean_total,
    variance = variance
  )
  
  plot <- ggplot(data) +
    geom_path(aes(variance, mean)) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_few() +
    theme(legend.position = c(0.75, 0.25))
  
  return(plot)
}
