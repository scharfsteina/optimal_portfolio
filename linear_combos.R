## Calculate the daily and cumulative returns of the portfolio using Markowitz weights,
## interval weights, and equal weights

get_linear_combos <- function(data, weights_markowitz, weights_interval) {
  date <- data[, 1]
  data <- data[, -1] %>% as.matrix()
  
  markowitz <- as.matrix(data) %*% as.matrix(weights_markowitz)
  interval <- as.matrix(data) %*% as.matrix(weights_interval)
  equal <- as.matrix(data) %*% as.matrix(rep(1, length(weights_markowitz)) / length(weights_markowitz)) # equally weighted
  
  cum_markowitz <- markowitz %>% 
    as_tibble() %>% 
    mutate(m_plus = V1 + 1,
           cumulative_markowitz = m_plus %>% cumprod()) %>% 
    select(cumulative_markowitz)
  
  cum_interval <- interval %>% 
    as_tibble() %>% 
    mutate(i_plus = V1 + 1,
           cumulative_interval = i_plus %>% cumprod()) %>% 
    select(cumulative_interval)
  
  cum_equal <- equal %>% 
    as_tibble() %>% 
    mutate(e_plus = V1 + 1,
           cumulative_equal = e_plus %>% cumprod()) %>% 
    select(cumulative_equal)
  
  returns <- bind_cols(date, as.vector(markowitz), as.vector(cum_markowitz), 
                       as.vector(interval), as.vector(cum_interval),
                       as.vector(equal), as.vector(cum_equal))
  colnames(returns) <- c("date", "markowitz","cumulative_markowitz", 
                         "interval", "cumulative_interval","equal", "cumulative_equal")
  
  return(returns)
}
