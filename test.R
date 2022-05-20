# TEST
library(rvest)

stocks <- c("GOOG","MSFT","CVS")

for (s in stocks) {
  html <- read_html(paste0("https://finance.yahoo.com/quote/",s,"/history"))
  data <- html_table(html)
  print(data)
}


