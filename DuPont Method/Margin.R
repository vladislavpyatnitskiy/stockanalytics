library("rvest") # Library

Margin <- function(x){ # Margin ratio
  
  margin <- NULL # List for Margin values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    p <- c("EBIT", "Total Revenue")
    
    c <- NULL  
    
    for (m in 1:length(p)){ c <- rbind(c, y[grep(p[m], y) + 1][1]) }
    
    c <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)))
    
    margin <- rbind(margin, c[1] / c[2]) } # EBIT / Revenue
    
  rownames(margin) <- x # Ticker names
  colnames(margin) <- "Margin (%)" # Column Name
  
  round(margin * 100, 2) # Display
}
Margin(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
