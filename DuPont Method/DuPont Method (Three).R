DuPont.three <- function(x){ # DuPont Method ratios
  
  dupont <- NULL # List for DuPont Method values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    bs<-sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is<-sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    c <- NULL
    h <- NULL
    
    p <- c("Total Assets", "Total Equity Gross Minority Interest")
    r <- c("Net Income Common Stockholders", "Total Revenue")
    
    for (m in 1:length(r)){ q <- NULL
    
      for (n in seq(1)){ q <- cbind(q, u[grep(r[m], u) + n])
      
      o <- NULL
      
      if (length(q) > 1){  o<-c(o,q[1]) } else if (length(q) == 1) { o<-q } } 
      
      c <- rbind(c, o) }
    
    for (m in 1:length(p)){ q <- NULL
    
      for (n in seq(1)){ q <- cbind(q, y[grep(p[m], y) + n])
      
      o <- NULL
      
      if (length(q) > 1){  o<-c(o,q[1]) } else if (length(q) == 1) { o<-q } } 
      
      h <- rbind(h, o) }
    
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Reduce commas
    h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) # Reduce commas
    
    d.ratios <- cbind(as.numeric(c[1]) / as.numeric(h[2]), # Return on Equity
                      as.numeric(c[1]) / as.numeric(c[2]), # Net Profit Margin
                      as.numeric(c[2]) / as.numeric(h[1]), # Asset Turnover
                      as.numeric(h[1]) / as.numeric(h[2])) # Equity Multiplier
    
    dupont <- rbind(dupont, d.ratios) } # DuPont Method
  
  rownames(dupont) <- x # Ticker names
  colnames(dupont) <- c("ROE (%)","Net Profit Margin (%)","Asset Turnover (%)",
                        "Equity Multiplier (%)") 
  
  round(dupont * 100, 2) # Display
}
DuPont.three(c("AAPL")) # Test