library("rvest")

Tax.burden <- function(x){ # Tax Burden ratio
  
  tb <- NULL # List for Tax Burden values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    p <- c("Net Income Common Stockholders", "Pretax Income")
    
    c <- NULL  
    
    for (m in 1:length(p)){ c <- rbind(c, y[grep(p[m], y) + 1][1]) }
    
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) 
    
    tb <- rbind(tb, as.numeric(c[1])/as.numeric(c[2])) } # Net/Pretax Incomes
    
  rownames(tb) <- x # Ticker names
  colnames(tb) <- "Interest Burden (%)" # Column Name
  
  round(tb * 100, 2) # Display
}
Tax.burden(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
