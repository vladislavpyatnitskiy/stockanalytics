library("rvest") # Library

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
    
    # Take values for Net Income and Equity
    c <- gsub(",","",gsub("([a-zA-Z]),","\\1 ",
                          u[grep("Net Income Common Stockholders",u)+1][1]))
    
    h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ",
                            y[grep("Total Equity Gross Minority Interest",
                                   y) + 1])) 
    
    roe <- rbind(roe, as.numeric(c) / as.numeric(h)) } # Net Income / Equity
    
  rownames(roe) <- x # Ticker names
  colnames(roe) <- "ROE (%)" # Column Name
  
  round(roe * 100, 2) # Display
}
ROE(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
