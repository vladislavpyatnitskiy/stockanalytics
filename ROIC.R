library("rvest") # Library

ROIC <- function(x){ # Return on Invested Capital
  
  bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s", x, x)
  is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", x, x)
  
  page.bs <- read_html(bs) # Read HTML & extract necessary info
  page.is <- read_html(is) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
  price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
  
  y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
  u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
  
  c <- NULL
  h <- NULL
  
  p <- c("Invested Capital")
  
  r <- c("Operating Income","Tax Provision", "Net Income Common Stockholders")
  
  for (m in 1:length(r)){ q <- NULL
  
    for (n in seq(1)){ q <- cbind(q, u[grep(r[m], u) + n])
    
    o <- NULL
    
    if (length(q) > 1){  o <- c(o, q[1]) 
    
      } else if (length(q) == 1) { o <- q } } 
    
    c <- rbind(c, o) }
    
  c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Reduce commas
  
  h <- y[grep(p, y) + 1]
  h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) # Reduce commas
  
  rownames(c) <- r # Give dates
  
  (1-as.numeric(c[2])/as.numeric(c[3]))*as.numeric(c[1])/as.numeric(h) 
}
ROIC("AAPL") # Test
