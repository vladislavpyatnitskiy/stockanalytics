library("rvest") # Library

Turnover <- function(x){ # Total Revenue / Total Assets
  
  trnvr <- NULL # List for Turnover values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    # Take values 
    c<-gsub(",","",gsub("([a-zA-Z]),","\\1 ",u[grep("Total Revenue",u)+1][1])) 
    h <- gsub(",","",gsub("([a-zA-Z]),","\\1 ",y[grep("Total Assets",y)+1])) 
    
    trnvr <- rbind(trnvr, as.numeric(c) / as.numeric(h)) } # Revenue / Assets
    
  rownames(trnvr) <- x # Ticker names
  colnames(trnvr) <- "Turnover (%)" # Column Name
  
  round(trnvr * 100, 2) # Display
}
Turnover(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
