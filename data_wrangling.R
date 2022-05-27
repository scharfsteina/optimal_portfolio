get_data <- function(.from, .to) {
  stocks <- html_table(read_html("https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"))[[2]]
  stocks <- stocks$Symbol
  data <- NULL
  
  get_stock <- function(.stock) {
    data <- loadSymbols(.stock, 
                      from = .from, 
                      to = .to, 
                      env = NULL) %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column("date") %>% 
      as.tibble() %>% 
      mutate(date = as.Date(date)) %>% 
      select(1, 5) %>% 
      setNames(c("date", "close")) %>% 
      mutate(percentage = (close - lag(close)) / lag(close)) %>% 
      filter(!is.na(percentage)) %>% 
      select(date, percentage)
    
    return(data)
  }
  
  for (i in seq_along(stocks)) {
    if (i == 1) {
      data <- get_stock(stocks[i])
    } 
    else {
      temp <- get_stock(stocks[i])$percentage
      data <- cbind(data, temp)
    }
  }
  
  names(data) <- c("date", stocks)
  data <- data %>%
    as_tibble()
}