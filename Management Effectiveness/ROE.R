ROE <- function(x){ # Return on Equity
  
  roe <- NULL # List for ROE values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    c <- NULL
    h <- NULL
    
    p <- c("Total Equity Gross Minority Interest")
    
    r <- c("Net Income Common Stockholders")
    
    for (m in 1:length(r)){ q <- NULL
    
      for (n in seq(1)){ q <- cbind(q, u[grep(r[m], u) + n])
      
      o <- NULL
      
      if (length(q) > 1){  o <- c(o,q[1]) } else if (length(q) == 1) { o<-q } } 
      
    c <- rbind(c, o) }
    
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Reduce commas
    h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", y[grep(p, y) + 1])) 
    
    roe <- rbind(roe, as.numeric(c) / as.numeric(h)) } # Net Income / Equity
    
  rownames(roe) <- x # Ticker names
  colnames(roe) <- "ROE (%)" # Column Name
  
  round(roe * 100, 2) # Display
}
ROE(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
