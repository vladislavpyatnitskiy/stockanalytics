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
    
    c <- NULL
    h <- NULL
    
    p <- c("Total Debt")
    r <- c("EBITDA")
    
    for (m in 1:length(r)){ q <- NULL
    
      for (n in seq(1)){ q <- cbind(q, u[grep(r[m], u) + n])
      
      o <- NULL
      
      if (length(q) > 1){  o <- c(o, q[1]) 
      
        } else if (length(q) == 1) { o <- q } } 
      
      c <- rbind(c, o) }
    
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Reduce commas
    h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", y[grep(p, y) + 1])) 
    
    rownames(c) <- r # Give dates
    
    EBITDA <- as.numeric(c) # EBITDA
    DEBT <- as.numeric(h) # DEBT
    
    db <- rbind(db, DEBT /EBITDA) } # Add values
    
  rownames(db) <- x # Ticker names
  colnames(db) <- "DEBT/EBITDA" # Column Name
  
  db # Display
}
Debt.EBITDA(c("AAPL", "MSFT", "META", "GOOGL", "NVDA", "AMZN")) # Test
