library("rvest")

Interest.burden <- function(x){ # Interest Burden ratio
  
  ib <- NULL # List for Interest Burden values
  
  for (q in 1:length(x)){ a <- x[q] # Each ticker in vector
  
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", a, a)
    
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    p <- c("Pretax Income", "EBIT")
    
    c <- NULL  
    
    for (m in 1:length(p)){ c <- rbind(c, y[grep(p[m], y) + 1][1]) }
    
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) 
    
    ib <- rbind(ib, as.numeric(c[1])/as.numeric(c[2])) } # Pretax Income/EBIT
    
  rownames(ib) <- x # Ticker names
  colnames(ib) <- "Interest Burden (%)" # Column Name
  
  round(ib * 100, 2) # Display
}
Interest.burden(c("AAPL", "MSFT", "AMZN", "GOOGL", "META", "NVDA")) # Test
