#source("data-wrangling.R")
get_linear_combos <- function(df, opt_weights) {
  date <- df[,1]
  df <- df[,-1] %>% as.matrix()
  markowitz <- as.matrix(df) %*% as.matrix(opt_weights)
  equal <- as.matrix(df) %*% as.matrix(rep(1,length(opt_weights))/length(opt_weights)) # equally weighted
  cum_markowitz <- markowitz %>% 
    as.tibble() %>% 
    mutate(m_plus = V1+1,
           cumulative_markowitz = m_plus %>% cumprod()) %>% 
    select(cumulative_markowitz)
  cum_equal <- equal %>% 
    as.tibble() %>% 
    mutate(e_plus = V1+1,
           cumulative_equal = e_plus %>% cumprod()) %>% 
    select(cumulative_equal)
  ret <- bind_cols(date, as.vector(markowitz), as.vector(cum_markowitz), as.vector(equal), as.vector(cum_equal))
  colnames(ret) <- c("date", "markowitz","cumulative_markowitz", "equal", "cumulative_equal")
  return(ret)
}


# plot_base <- function(final) {
#   plot(final$date,
#        final$markowitz,
#        col = "red",
#        main = "Comparing Portfolio Allocation Strategies of Dow 30",
#        type = "l",
#        ylim = c(min(final$markowitz,final$equal)-.25,max(final$markowitz,final$equal)+.25),
#        xlab = "2022",
#        ylab = "",
#        lwd = 2,
#        xaxt = "n")
#   axis(1, at = seq(as.Date("2022/01/01"), as.Date("2022/06/01"), by = "month"))
#   lines(final$date, final$equal, col = "blue")
#   legend("bottomleft", c("Optimal Portfolio Allocation",
#                          "Equal Portfolio Allocation"),
#          lty = c(1,1),
#          lwd = c(2,1),
#          col = c("red","blue"),
#          seg.len = 1,
#          text.width = 28,
#          x.intersp = .25)
# }

plot_daily <- function(final) {
  final %>%
    pivot_longer(values_to = "returns", names_to = "method", cols = c(markowitz,equal)) %>%
    ggplot(aes(x = date, y = returns, color = method)) +
    geom_line() +
    #geom_smooth() +
    expand_limits(y = c(-1, 1)) +
    scale_color_manual(labels  =c("Equal Weights", "Markowitz"), values = c("blue", "red")) +
    labs(title = "Comparing Returns of Portfolio Allocation Strategies on Dow 30",
         x = "Date",
         y = "Return (in %)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "bottom")
}

plot_cumulative <- function(final) {
  final %>%
    pivot_longer(values_to = "returns", 
                 names_to = "method", 
                 cols = c(cumulative_markowitz, 
                          cumulative_equal)) %>%
    ggplot(aes(x = date, y = returns, color = method)) +
    geom_line() +
    #geom_smooth() +
    expand_limits(y = c(-1, 1)) +
    scale_color_manual(labels  =c("Equal Weights", "Markowitz"), values = c("blue", "red")) +
    labs(title = "Comparing Returns of Portfolio Allocation Strategies on Dow 30",
         x = "Date",
         y = "Return (in %)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "bottom")
}

# current_data <- get_data(.from = make_date(2022,1,1), .to = today())
# final <- get_linear_combos(current_data, rnorm(length(current_data)-1))
# 
# plot_daily(final)
# plot_cumulative(final)
