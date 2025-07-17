lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

MAE <- function(x, s = NULL, e = NULL, data = T){ # Mean Absolute Error
  
  if (data){ p <- NULL # Create an empty variable
    
    src <- "yahoo"
    
    getData <- function(A, s, e) {
      if (is.null(s) && is.null(e)) return(getSymbols(A,src=src,auto.assign=F)) 
      if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
      if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
      return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
    }
    for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) } # Join data
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in data set
    
    x <- as.timeSeries(p) } # Make it time series
    
  l <- NULL
  
  for (n in 1:ncol(x)){ s <- x[,n]
  
    l <- rbind(l, sum(abs(s - mean(s))) / length(s)) }
  
  rownames(l) <- colnames(x)
  colnames(l) <- "MAE"
  
  l
}
MAE(c("AAPL", "C"), s = "2020-01-01", data = T) # Test
