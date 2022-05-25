#source("data-wrangling.R")
#library(tidyverse)
#library(ggthemes)
get_linear_combos <- function(df, opt_weights) {
  date <- df[,1]
  df <- df[,-1] %>% as.matrix()
  markowitz <- as.matrix(df) %*% as.matrix(opt_weights)
  equal <- as.matrix(df) %*% as.matrix(rep(1,lengths(opt_weights))/length(opt_weights)) # equally weighted
  return(bind_cols(date,markowitz, equal))
}

colnames(final) <- c("date", "markowitz", "equal")

plot(final$date,
     final$markowitz,
     col = "red",
     main = "Comparing Portfolio Allocation Strategies of Dow 30",
     type = "l",
     ylim = c(min(final$markowitz,final$equal)-.25,max(final$markowitz,final$equal)+.25),
     xlab = "2022",
     ylab = "",
     lwd = 2)
lines(final$date, final$equal, col = "blue")
legend("bottomleft", c("Optimal Portfolio Allocation",
                       "Equal Portfolio Allocation"),
       lty = c(1,1),
       lwd = c(2,1),
       col = c("red","blue"),
       seg.len = 1,
       text.width = 28,
       x.intersp = .25)
