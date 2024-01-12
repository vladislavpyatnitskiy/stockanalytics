library("rvest")

Debt.EBITDA <- function(x){ # Debt to EBITDA
  
  db <- NULL # List for Debt to EBITDA values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
    
    bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    # Find values of Debt and EBITDA  
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", u[grep("EBITDA", u)+1][1])) 
    h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", y[grep("Total Debt",y)+1])) 
    
    db <- rbind(db, as.numeric(h) / as.numeric(c)) } # Add values
    
  rownames(db) <- x # Ticker names
  colnames(db) <- "DEBT/EBITDA" # Column Name
  
  db # Display
}
Debt.EBITDA(c("AAPL", "MSFT", "META", "GOOGL", "NVDA", "AMZN")) # Test
