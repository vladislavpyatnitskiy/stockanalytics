lapply(c("quantmod","timeSeries"),require,character.only=T) # Libraries

# Script to calculate Sharpe Ratio
sharpe.ratio <- function(y, tr = "^TNX", s = NULL, e = NULL){
  
  y <- c(y, tr) # Add 10 year Treasuries to list
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  src <- "yahoo"
  
  getData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A, src=src, auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  for (A in y){ p <- cbind(p, getData(A, s, e)[,4]) 
  
    message(sprintf("%s is downloaded; %s from %s", A, which(y==A), length(y)))
  
  } # Join data
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  rf <- apply(p[,tr], 2, function(col) mean(col)) # Risk Free
  
  # Matrix of financial instruments & Turn to logs & Calculate Mean and SD
  r = apply(
    diff(log(p[,-which(names(p) == tr)]))[-1,], 2,
    function(col) c((exp(sum(col)) - 1) * 100, sd(col) * 1000)
    )
  
  S <- NULL # List for Sharpe values
  
  # Calculate Sharpe ratio & Put values into list
  for (k in 1:ncol(r)) S <- rbind(S, (r[1,k] - rf) / r[2,k])
  
  rownames(S) <- colnames(r) # Add tickers
  colnames(S) <- "Sharpe" # Put name of ratio to column
  
  S # Display values
}
sharpe.ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), s = "2026-01-01") # Test
