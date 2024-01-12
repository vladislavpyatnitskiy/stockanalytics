library("rvest") # Library

ROA <- function(x){ # Return on Assets
  
  roa <- NULL # List for ROA values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    # Take values for EBIT and Total Assets, clean them and make numeric
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", u[grep("EBIT", u) + 1][1])) 
    h <- gsub(",", "", gsub("([a-zA-Z]),","\\1 ",y[grep("Total Assets",y)+1])) 
    
    roa <- rbind(roa, as.numeric(c) / as.numeric(h)) } # EBIT / Total Assets
    
  rownames(roa) <- x # Ticker names
  colnames(roa) <- "ROA (%)" # Column Name
  
  round(roa * 100, 2) # Display
}
ROA(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
