# Libraries 
lapply(c("quantmod", "timeSeries"), require, character.only = TRUE)

# Script to calculate Sharpe Ratio
sharpe_ratio <- function(y, tr = "^TNX", s = NULL, e = NULL){
  
  y <- c(y, tr) # Add 10 year Treasuries to list
  
  p <- NULL # Create an empty variable
  
  # Loop for data extraction & Set up statements for start and end dates
  for (Ticker in y){ if (is.null(s) && is.null(e)) {
    
    # When neither start date nor end date are defined
    p<-cbind(p,getSymbols(Ticker,from=as.Date(Sys.Date())-365,to=Sys.Date(),
                          src="yahoo",auto.assign=F)[,4])
    
  } else if (is.null(e)) { # When only start date is defined
    
    p <- cbind(p, getSymbols(Ticker,from=s,src="yahoo",auto.assign=F)[,4])
    
  } else if (is.null(s)) { # When only end date is defined
    
    p <- cbind(p, getSymbols(Ticker,to=e,src = "yahoo",auto.assign=F)[,4])
    
  } else { # When both start date and end date are defined
    
    p<-cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])} }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  r <- as.timeSeries(p) # Make it time series
  
  rf <- apply(r[,tr], 2, function(col) mean(diff(log(col)))) # Risk Free
  
  # Matrix of financial instruments & Turn to logs & Calculate Mean and SD
  v<-apply(diff(log(r[,-which(names(r)==tr)]))[-1,],2,function(x) c(mean(x),
                                                                    sd(x)))
  df.sharpe <- NULL # List for Sharpe values
  
  # Calculate Sharpe ratio & Put values into list
  for (k in 1:ncol(v)){ df.sharpe<-rbind(df.sharpe,(v[1,k]-rf)*252^.5/v[2,k]) }

  rownames(df.sharpe) <- tickers # Add tickers
  colnames(df.sharpe) <- "Sharpe" # Put name of ratio to column
  
  return(df.sharpe) # Display values
}
# Test
sharpe_ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), s = "2022-09-01")
