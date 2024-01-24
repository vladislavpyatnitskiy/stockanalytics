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
    
    for (m in 1:length(r)){ c <- rbind(c, u[grep(r[m], u) + 1][1]) }
    
    p <- c("Invested Capital", "Total Assets", 
           "Total Equity Gross Minority Interest")
    
    h <- NULL
    
    for (m in 1:length(p)){ h <- rbind(h, y[grep(p[m], y) + 1][1]) }
    
    c <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)))
    h <- as.numeric(gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)))
    
    ratios <- rbind(ratios, cbind((1 - c[2] / c[3]) * c[1] / h[1], # ROIC
                            c[4] / h[2], # EBIT / Total Assets
                            c[3] / h[3])) } # Net Income / Equity
    
  rownames(ratios) <- x # Ticker names
  colnames(ratios) <- c("ROIC (%)", "ROA (%)", "ROE (%)") # Column Name
  
  round(ratios * 100, 2) # Display
}
Return.ratios(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
