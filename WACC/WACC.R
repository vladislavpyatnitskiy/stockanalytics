lapply(c("quantmod","timeSeries","rvest"),require,character.only=T) # Libraries

WACC <- function(x,  tr = "^TNX", i = "^GSPC", N = 10){ # WACC
  
  wacc <- NULL # List for values
  
  for (g in 1:length(x)){ a <- x[g] # Assign variable for each ticker
  
    bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s",a,a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    page.is <- read_html(is) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    c <- NULL
    h <- NULL
    
    d <- c("Total Debt", "Total Liabilities Net Minority Interest",
           "Total Equity Gross Minority Interest")
    
    r <- c("Interest Expense","Tax Provision","Net Income Common Stockholders")
    
    for (m in 1:length(r)){ c <- rbind(c, u[grep(r[m], u) + 1][1]) }
    for (m in 1:length(d)){ h <- rbind(h, y[grep(d[m], y) + 1][1]) }
    
    c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Income Statement
    h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) # Balance Sheet
    
    Rd <- as.numeric(c[1]) / as.numeric(h[1]) # Interest Expense / Total Debt
    
    # Tax Provision / Net Income Common Stockholders
    after.tax.ratio <- 1 - as.numeric(c[2]) / as.numeric(c[3]) 
    
    # Liabilities / Total Assets
    debt.part <- as.numeric(h[2]) / (as.numeric(h[2]) + as.numeric(h[3]))
    
    costofdebt <- debt.part * Rd * after.tax.ratio # Display
    
    capital.part <- as.numeric(h[3]) / (as.numeric(h[2]) + as.numeric(h[3]))
    
    actual.date <-as.Date(as.character(y[grep("Breakdown", y) + 1]),"%m/%d/%Y")
    
    yi <- c(a, tr, i) # Add 10 year Treasuries to list
    
    p <- NULL # Create a list for securities data
    
    for (A in yi){ p <- cbind(p, getSymbols(A, from = actual.date - 365 * N, 
                                            to = actual.date, src = "yahoo",
                                            auto.assign=F)[,4]) }
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- yi # Put the tickers in data set
    
    p <- as.timeSeries(p) # Make it time series
    
    rf <- apply(p[,tr], 2, function(col) mean(col) / 100) # Risk Free Return
    
    # Calculate  beta
    b <- apply(diff(log(p[,a]))[-1,], 2,
               function(col) (lm((col)~diff(log(p[,i]))[-1,]))$coefficients[2])
    
    ER <- rf + b * ((exp(sum(diff(log(p[,i]))[-1,])))^(1 / N) - 1 - rf) # CAPM
    
    wacc <- rbind(wacc, costofdebt + as.numeric(capital.part * ER)) } # Display
    
  rownames(wacc) <- x # Row names
  colnames(wacc) <- "WACC (%)" # Column names
  
  round(wacc * 100, 2) # Return
}
WACC(c("MU", "NVDA"), i = "^GSPC") # Test
