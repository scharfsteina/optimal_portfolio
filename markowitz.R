## Calculate the portfolio weights based on the Markowitz model both pairwise and
## for all stocks


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
