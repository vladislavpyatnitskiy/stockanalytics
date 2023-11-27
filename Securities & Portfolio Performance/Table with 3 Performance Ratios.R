lapply(c("quantmod","timeSeries"),require,character.only=T) # Libraries

perform.ratio <- function(x, tr = "^TNX", spx = "^GSPC",
                          s = as.Date(Sys.Date())-365, e=as.Date(Sys.Date())){
  
  y <- c(x, tr, spx) # Add 10 year Treasuries to list
  
  p <- NULL # Create an empty variable
  
  for (A in y) p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo", # Daily data
                                     auto.assign=F)[,4])
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  rf <- apply(p[,tr], 2, function(col) mean(col)) # Risk Free
  
  # Matrix of financial instruments & Turn to logs & Calculate Mean and SD
  r<-apply(diff(log(p[,1:length(x)]))[-1,],2,
           function(col) c((exp(sum(col)) - 1) * 100, sd(col) * 1000))
  
  b<-apply(diff(log(p[,1:length(x)]))[-1,], 2, # Beta
           function(col) ((lm((col)~diff(log(p[,spx]))[-1,]))$coefficients[2]))
  
  r.mean <- apply(r, 2, function(col) mean(col)) # Calculate return
  
  perf.df <- NULL # List for Sharpe, Treynor & Sortino values

  for (k in 1:length(x)){ sharpe <- (r[1,k] - rf) / r[2,k] # Sharpe
  
    treynor <- (r[1,k] - rf) / b[k] / 100 # Treynor
  
    excess.r <- r[,k] - r.mean[k] # Sortino
  
    Sortino <- (mean(excess.r) - rf) / mean((excess.r[excess.r < 0]) ^ 2) ^ .5
    
    perf.v <- cbind.data.frame(sharpe, treynor, Sortino) # All measures
    
    rownames(perf.v) <- x[k] # Ticker
    
    perf.df <- rbind.data.frame(perf.df, perf.v) } # Join all 
  
  colnames(perf.df) <- c("Sharpe", "Treynor", "Sortino") 
  
  return(perf.df) # Display values
}
# Test
perform.ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), s = "2022-09-01")
