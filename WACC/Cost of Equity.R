lapply(c("quantmod","timeSeries","rvest"),require,character.only=T) # Libs

cost.of.equity <- function(x, tr = "^TNX", i = "^GSPC",N=10){ # Cost of Equity
  
  l <- NULL
  
  for (n in 1:length(x)){ a <- x[n]
      
    bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",a,a)
    
    page.bs <- read_html(bs) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
    
    y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
    
    h <- NULL
    
    d <- c("Total Debt", "Total Liabilities Net Minority Interest",
           "Total Equity Gross Minority Interest")
    
    for (m in 1:length(d)){ h <- rbind(h, y[grep(d[m], y) + 1][1]) }
    
    h <- as.numeric(gsub(",","", gsub("([a-zA-Z]),","\\1 ",h))) # Balance Sheet
    
    capital.part <- h[3] / (h[2] + h[3])
    
    today <- as.Date(as.character(y[grep("Breakdown",y) + 1]), "%m/%d/%Y")
    
    yi <- c(a, tr, i) # Add 10 year Treasuries to list
    
    p <- NULL # Create a list for securities data
    
    for (A in yi){ p <- cbind(p, getSymbols(A, from = today - 365 * N, 
                                            to = today, src = "yahoo",
                                            auto.assign=F)[,4]) }
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- yi # Put the tickers in data set
    
    p <- as.timeSeries(p) # Make it time series
    
    rf <- apply(p[,tr], 2, function(col) mean(col) / 100) # Risk Free Return
    
    # Calculate  beta
    b <- apply(diff(log(p[,a]))[-1,], 2,
               function(col) (lm((col) ~
                                   diff(log(p[,i]))[-1,]))$coefficients[2])
    
    ER <- rf + b * ((exp(sum(diff(log(p[,i]))[-1,])))^(1 / N) - 1 - rf) # CAPM
    
    l <- rbind.data.frame(l, as.numeric(capital.part * ER)) } # Display
  
  rownames(l) <- x
  colnames(l) <- "Cost of Equity"
  
  l
}
cost.of.equity(c("AAPL", "MSFT")) # Test
