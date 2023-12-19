library("rvest") # Library

cost.of.debt <- function(x){ # Cost of Debt
  
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
  
  p <- c("Total Debt", "Total Liabilities Net Minority Interest",
         "Total Equity Gross Minority Interest")
  
  r <- c("Interest Expense", "Tax Provision", "Net Income Common Stockholders")
  
  for (m in 1:length(r)){ q <- NULL
  
    for (n in seq(1)){ q <- cbind(q, u[grep(r[m], u) + n])
    
    o <- NULL
    
    if (length(q) > 1){  o <- c(o, q[1]) 
    
      } else if (length(q) == 1) { o <- q } } 
    
    c <- rbind(c, o) }
    
  c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Income Statement
  
  for (m in 1:length(p)){ v <- NULL
  
    for (n in seq(1)){ v <- cbind(v, y[grep(p[m], y) + n])
    
    w <- NULL
    
    if (length(v) > 1){ for (n in seq(0,3,1)) w <- c(w, v[1 + 2*n]) 
    
      } else if (length(v) == 1) { w <- v } } 
    
    h <- rbind(h, w) }
    
  h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) # Balance Sheet
  
  Rd <- as.numeric(c[1]) / as.numeric(h[1]) # Interest Expense / Total Debt
  
  # Tax Provision / Net Income Common Stockholders
  after.tax.ratio <- 1 - as.numeric(c[2]) / as.numeric(c[3]) 
  
  # Liabilities / Total Assets
  debt.part <- as.numeric(h[2]) / (as.numeric(h[2]) + as.numeric(h[3]))
  
  debt.part * Rd * after.tax.ratio # Display
}
cost.of.debt("AAPL") # Test