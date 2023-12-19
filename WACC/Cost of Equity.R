cost.of.equity <- function(x,  tr = "^TNX", i = "^GSPC"){ # Cost of Equity
  
  bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s", x, x)
  
  page.bs <- read_html(bs) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
  
  y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
  
  h <- NULL
  
  d <- c("Total Debt", "Total Liabilities Net Minority Interest",
         "Total Equity Gross Minority Interest")
  
  for (m in 1:length(d)){ v <- NULL
  
    for (n in seq(1)){ v <- cbind(v, y[grep(d[m], y) + n])
    
    w <- NULL
    
    if (length(v) > 1){ for (n in seq(0,3,1)) w <- c(w, v[1 + 2*n]) 
    
      } else if (length(v) == 1) { w <- v } } 
    
    h <- rbind(h, w) }
  
  h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) # Balance Sheet
  
  capital.part <- as.numeric(h[3]) / (as.numeric(h[2]) + as.numeric(h[3]))
  
  actual.date <-as.Date(as.character(y[grep("Breakdown", y) + 1]), "%m/%d/%Y")
  
  yi <- c(x, tr, i) # Add 10 year Treasuries to list
  
  p <- NULL # Create a list for securities data
  
  for (A in yi){ p <- cbind(p, getSymbols(A, from = actual.date - 365, 
                                         to = actual.date,
                                         src = "yahoo", auto.assign=F)[,4]) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- yi # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  rf <- apply(p[,tr], 2, function(col) mean(col) / 100) # Risk Free Return
  
  rm <- exp(sum(diff(log(p[,i]))[-1,])) # Markter Return
  
  # Calculate  beta
  b <- apply(diff(log(p[,x]))[-1,], 2,
             function(col) (lm((col) ~ diff(log(p[,i]))[-1,]))$coefficients[2])
  
  ER <- rf + b * (rm - rf) # CAPM
  
  as.numeric(capital.part * ER) # Display
}
cost.of.equity("AAPL") # Test
