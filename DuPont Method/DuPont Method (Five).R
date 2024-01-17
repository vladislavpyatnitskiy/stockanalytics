library("rvest") # library

DuPont.five <- function(x){ # DuPont Method ratios
  
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
    r <- c("Net Income Common Stockholders", "Pretax Income", "EBIT",
           "Total Revenue")
    
    for (m in 1:length(r)){ c <- rbind(c, u[grep(r[m], u) + 1][1]) }
      
    for (m in 1:length(p)){ h <- rbind(h, y[grep(p[m], y) + 1][1]) }
      
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Reduce commas
    h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) 
    
    d.ratios <- cbind(as.numeric(c[1]) / as.numeric(h[2]), # Return on Equity
                      as.numeric(c[1]) / as.numeric(c[2]), # Tax Burden
                      as.numeric(c[2]) / as.numeric(c[3]), # Interest Burden
                      as.numeric(c[3]) / as.numeric(c[4]), # Margin
                      as.numeric(c[4]) / as.numeric(h[1]), # Turnover
                      as.numeric(h[1]) / as.numeric(h[2])) # Equity Multiplier
    
    dupont <- rbind(dupont, d.ratios) } # DuPont Method
  
  rownames(dupont) <- x # Ticker names
  colnames(dupont) <- c("ROE (%)", "Tax Burden (%)", "Interest Burden (%)",
                        "Margin (%)","Turnover (%)","Equity Multiplier (%)") 
  
  round(dupont * 100, 2) # Display
}
DuPont.five(c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "NVDA", "TSLA")) # Test
