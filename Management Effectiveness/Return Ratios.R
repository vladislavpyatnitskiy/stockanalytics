library("rvest") # Library

Return.ratios <- function(x){ ratios <- NULL # List for values
  
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
    
    r <- c("Operating Income","Tax Provision","Net Income Common Stockholders",
           "EBIT")
    
    for (m in 1:length(r)){ q <- NULL
    
      for (n in seq(1)){ q <- cbind(q, u[grep(r[m], u) + n])
      
      o <- NULL
      
      if (length(q) > 1){  o <- c(o,q[1]) } else if (length(q) == 1) { o<-q } } 
    
    c <- rbind(c, o) }
    
    p <- c("Invested Capital", "Total Assets", 
           "Total Equity Gross Minority Interest")
    
    h <- NULL
    
    for (m in 1:length(p)){ q <- NULL
    
      for (n in seq(1)){ q <- cbind(q, y[grep(p[m], y) + n])
      
      o <- NULL
      
      if (length(q) > 1){  o <- c(o,q[1]) } else if (length(q) == 1) { o<-q } } 
      
      h <- rbind(h, o) }
    
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Reduce commas
    h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) # Reduce commas 
    
    oi <- as.numeric(c[1]) # Operating Income
    tp <- as.numeric(c[2]) # Tax Provision
    ni <- as.numeric(c[3]) # Net Income
    ebit <- as.numeric(c[4]) # EBIT
    ic <- as.numeric(h[1]) # Investing Capital
    ta <- as.numeric(h[2]) # Total Assets
    eq <- as.numeric(h[3]) # Equity
    
    # Add values
    ratios <- rbind(ratios, cbind((1 - tp/ni) * oi/ic, ebit/ta, ni/eq)) }
    
  rownames(ratios) <- x # Ticker names
  colnames(ratios) <- c("ROIC (%)", "ROA (%)", "ROE (%)") # Column Name
  
  round(ratios * 100, 2) # Display
}
Return.ratios(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
