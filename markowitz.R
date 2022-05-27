## Calculate the portfolio weights based on the Markowitz model both pairwise and
## for all stocks

# Pairwise Markowitz
get_pairwise_markowitz <- function(data) {
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


# Optimal weights using Markowitz model
get_weights_markowitz <- function(data, returns) {
  mu <- colMeans(data)
  omega <- cov(data)
  
  inv_omega <- solve(omega)
  A <- (t(mu) %*% inv_omega %*% rep(1, length(mu)))[1,1]
  B <- (t(mu) %*% inv_omega %*% mu)[1,1]
  C <- (t(rep(1, length(mu))) %*% inv_omega %*% rep(1, length(mu)))[1,1]
  D <- (B * C - A^2)
  a <- ((returns * C - A) / D)
  
  w <- inv_omega %*% (a * mu + ((1 - a * A) / C) * rep(1, length(mu)))
  
  return(w)
}
