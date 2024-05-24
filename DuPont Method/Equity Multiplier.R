library("rvest") # Library

Equity.multiplier <- function(x){ # Equity Multiplier
  
  em <- NULL # List for Equity Multiplier values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
    
    bs<-sprintf("https://uk.finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    
    tab.bs <- page.bs %>% html_nodes('div') %>% .[[1]]
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    p <- c("Total assets", "Total stockholders' equity")
    
    c <- NULL  
    
    for (m in 1:length(p)){ c <- rbind(c, y[grep(p[m], y) + 1][1]) }
    
    c <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)))
    
    em <- rbind(em, c[1] / c[2]) } # Total Assets / Equity
    
  rownames(em) <- x # Ticker names
  colnames(em) <- "Equity Multiplier (%)" # Column Name
  
  round(em * 100, 2) # Display
}
Equity.multiplier(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
