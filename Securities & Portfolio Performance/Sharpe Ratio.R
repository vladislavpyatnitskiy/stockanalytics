lapply(c("quantmod","timeSeries"),require,character.only=T) # Libraries

# Script to calculate Sharpe Ratio
sharpe.ratio <- function(y, tr = "^TNX", s = as.Date(Sys.Date())-365,
                         e = as.Date(Sys.Date())){
  
  y <- c(y, tr) # Add 10 year Treasuries to list
  
  p <- NULL # Create an empty variable
  
  for (A in y) p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo", # Daily data
                                     auto.assign=F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  rf <- apply(p[,tr], 2, function(col) mean(col)) # Risk Free
  
  # Matrix of financial instruments & Turn to logs & Calculate Mean and SD
  r<-apply(diff(log(p[,-which(names(p)==tr)]))[-1,],2,
           function(col) c((exp(sum(col)) - 1) * 100, sd(col) * 1000))
  
  sharpe <- NULL # List for Sharpe values
  
  # Calculate Sharpe ratio & Put values into list
  for (k in 1:ncol(r)){ sharpe <- rbind(sharpe, (r[1,k] - rf) / r[2,k]) }
  
  rownames(sharpe) <- colnames(r) # Add tickers
  colnames(sharpe) <- "Sharpe" # Put name of ratio to column
  
  return(sharpe) # Display values
}
# Test
sharpe.ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), s = "2022-09-01")
