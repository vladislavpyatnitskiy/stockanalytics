library("rvest") # Library

ROIC <- function(x){ # Return on Invested Capital
  
  roic <- NULL # List for ROIC values
  
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
    
    r <- c("Operating Income","Tax Provision","Net Income Common Stockholders")
    
    for (m in 1:length(r)){ c <- rbind(c, u[grep(r[m], u) + 1][1]) }
    
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Reduce commas
    h<-gsub(",","",gsub("([a-zA-Z]),","\\1 ",y[grep("Invested Capital",y)+1])) 
    
    oi <- as.numeric(c[1]) # Operating Income
    tp <- as.numeric(c[2]) # Tax Provision
    ni <- as.numeric(c[3]) # Net Income
    ic <- as.numeric(h) # Investing Capital
    
    roic <- rbind(roic, (1 - tp / ni) * oi / as.numeric(h)) } # Add values
    
  rownames(roic) <- x # Ticker names
  colnames(roic) <- "ROIC (%)" # Column Name
  
  round(roic * 100, 2) # Display
}
ROIC(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
