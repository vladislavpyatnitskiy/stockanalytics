library("rvest") # Library

cost.of.debt <- function(x){ # Cost of Debt
  
  l <- NULL
  
  for (n in 1:length(x)){ a <- x[n]
  
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
    
    p <- c("Total Debt", "Total Liabilities Net Minority Interest",
           "Total Equity Gross Minority Interest")
    
    r <- c("Interest Expense","Tax Provision","Net Income Common Stockholders")
    
    for (m in 1:length(r)){ c <- rbind(c, u[grep(r[m], u) + 1][1]) }
    for (m in 1:length(p)){ h <- rbind(h, y[grep(p[m], y) + 1][1])  }
    
    c<-as.numeric(gsub(",","",gsub("([a-zA-Z]),","\\1 ",c))) # Income Statement
    h<-as.numeric(gsub(",","",gsub("([a-zA-Z]),","\\1 ",h))) # Balance Sheet
    
    Rd <- c[1] / h[1] # Interest Expense / Total Debt
    after.tax.ratio <- 1 - c[2] / c[3] # Tax Provision / Net Income 
    debt.part <- h[2] / (h[2] + h[3]) # Liabilities / Total Assets
    
    l <- rbind.data.frame(l, debt.part * Rd * after.tax.ratio) } # Display
  
  rownames(l) <- x
  colnames(l) <- "Cost of Debt"
  
  l # Display data frame
}
cost.of.debt(c("AAPL", "MSFT", "NVDA", "TSLA")) # Test
