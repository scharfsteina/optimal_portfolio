cor_heatmap <- function(data) {
  data %>%
    select(-date) %>% 
    cor() %>% 
    melt() %>% 
    ggplot(mapping = aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(mapping = aes(label = round(value, digits=1)),
              size = 3)+
    labs(x = NULL,
         y = NULL,
         title = "Correlation Heatmap of Dow 30",
         fill = "Correlation") +
    scale_fill_gradient(low = "#B81313", 
                        high = "#002DB5",
                        limits = c(0,1)) +
    theme(plot.title = element_text(hjust = 0.5))
    
}

pcor_heatmap <- function(data) {
  ic1 <- ic2  <- data %>%
    select(-date) %>% 
    cor() %>% 
    solve()
  for(i in 1:30){
    for(j in 1:30){
      ic1[i,j]=-ic2[i,j]/sqrt(abs(ic2[i,i]*ic2[j,j]))
    }
  }
  ic1 %>% 
    melt() %>% 
    ggplot(mapping = aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(mapping = aes(label = round(value, digits=1)),
              size = 3)+
    labs(x = NULL,
         y = NULL,
         title = "Partial Correlation Heatmap of Dow 30",
         fill = "Correlation") +
    scale_fill_gradient(low = "#B81313", 
                        high = "#002DB5",
                        limits = c(-1,1)) +
    theme(plot.title = element_text(hjust = 0.5))
}
