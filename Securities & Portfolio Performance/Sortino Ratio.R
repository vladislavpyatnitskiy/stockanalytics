# Libraries
lapply(c("quantmod", "timeSeries"), require, character.only = TRUE)

# Function to calculate Sortino Ratio
sortino.ratio <- function(y,tr10 = "^TNX",s = NULL,e = NULL,v.sortino = NULL){
  
  y <- c(y, tr10) # Add 10 year Treasuries to list
  
  p <- NULL # Create an empty variable
  
  for (Ticker in y){ if (is.null(s) && is.null(e)) { # Data upload
    
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
  
  r <- diff(log(as.timeSeries(p)))[-1,] # Clean data for returns
  
  rf <- apply(r[,tr10], 2, function(col) mean(col)) # Risk Free Rate
  
  s <- r[, -which(names(r) == tr10)] # Matrix with financial instruments
  
  r.mean <- apply(s, 2, function(col) mean(col)) # Calculate return
  
  # Calculate Sortino ratio and calculate excess returns
  for (k in 1:ncol(s)){ r.premium <- s[,k] - r.mean[k]
    
    e.r <- mean(s[,k] - r.mean[k]) # Mean of Excessive returns
    
    n.r <- (r.premium[r.premium < 0]) ^ 2 # Negative excessive returns and ^2
    
    # Calculate Downside Risk, Sortino ratio and add to list
    v.sortino<-rbind(v.sortino, ((e.r - rf)/(sum(n.r)/nrow(s))^.5) * 252^.5) }
    
  rownames(v.sortino) <- colnames(s) # Add tickers
  colnames(v.sortino) <- "Sortino" # Put name of ratio to column
  
  return(v.sortino) # Display values
}
# Test
sortino.ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), s = "2022-09-01")
