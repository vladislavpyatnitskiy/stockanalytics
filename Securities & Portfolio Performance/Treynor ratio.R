lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Treynor script
treynor.ratio <- function(x, tr10 = "^TNX", spx = "^GSPC",
                          s = as.Date(Sys.Date())-365, e=as.Date(Sys.Date())){
  
  y <- c(x, tr10, spx) # Add 10 year Treasuries to list
  
  p <- NULL # Create a list for securities data
  
  for (A in y){ p <- cbind(p, getSymbols(A, from = s, to = e,
                                         src = "yahoo", auto.assign=F)[,4]) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  rf <- apply(p[,tr10], 2, function(col) mean(col) / 100) # Risk Free Return
  
  # Expected Return
  r <- apply(diff(log(p[,1:length(x)]))[-1,],2,function(col) (exp(sum(col))-1))
  
  b<-apply(diff(log(p[,1:length(x)]))[-1,], 2, # Beta
           function(col) ((lm((col)~diff(log(p[,spx]))[-1,]))$coefficients[2]))
  
  treynor <- NULL # Calculate Treynor values
              
  for (k in 1:length(x)){ treynor <- rbind(treynor, (r[k] - rf) / b[k]) }
  
  rownames(treynor) <- names(r) # Add tickers
  colnames(treynor) <- "Treynor" # Put name of ratio to column
  
  return(treynor) # Display values
}
# Test
treynor.ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), s = "2022-09-01")
