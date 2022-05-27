interval <- function(r, R, mu, omega, returns) {
  inv_omega <- solve(omega)
  A <- (t(mu) %*% inv_omega %*% rep(1, length(mu)))[1,1]
  B <- (t(mu) %*% inv_omega %*% mu)[1,1]
  C <- (t(rep(1, length(mu))) %*% inv_omega %*% rep(1, length(mu)))[1,1]
  D <- (B * C - A^2)
  
  P <- (R * C - A) / D
  p <- (r * C - A) / D
  Q <- 1 / D
  
  func_a <- function(a, mu, omega, p, P, C, Q) {
    result = pnorm((P - a) / sqrt(C * (Q + a^2))) - pnorm(p - a) / sqrt(C * (Q + a^2))
    return(result)
  }
  
  optimal_a <- optimize(func_a, c(p, P), mu = mu, omega = omega, p = p, P = P, C = C,
                        Q = Q, tol = 0.001, maximum = TRUE)
  a <- optimal_a$maximum
  w <- inv_omega %*% (a * mu + ((1 - a * A) / C) * rep(1, length(mu)))
  return(w)
}