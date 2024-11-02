lapply(c("quantmod","timeSeries","fBasics"),require,character.only=T) # Libs

fin.stats <- function(x, lg = F, data = F, transpose = F, s = NULL, e = NULL){ 
  
  p <- NULL
  
  if (isTRUE(data)){ for (A in x){ if (is.null(s) && is.null(e)){
    
        q <- getSymbols(A, src = "yahoo", auto.assign=F)[,4]
    
        } else if (is.null(e)){ # When only start date is defined
    
        q <- getSymbols(A, from = s, src = "yahoo", auto.assign = F)[,4]
    
        } else if (is.null(s)){ 
    
        q <- getSymbols(A, to = e, src = "yahoo", auto.assign = F)[,4] } else { 
      
        q <- getSymbols(A, from=s, to=e, src="yahoo", auto.assign=F)[,4] } 
    
      p <- cbind(p, q) }
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in data set
    
    x <- as.timeSeries(p) } # Make it time series
  
  if (isTRUE(lg)){ x = diff(log(x))[-1,] } # logs & remove NA if needed
  
  # Calculate necessary statistics
  d <- t(apply(x, 2, function(x) c(min(x), quantile(x,na.rm=T,probs = c(.1)),
                                   quantile(x, na.rm=T,probs=c(.25)),
                                   median(x), quantile(x,na.rm=T,probs=c(.75)),
                                   quantile(x,na.rm=T, probs = c(.9)), max(x),
                                   mean(x), var(x), sd(x), skewness(x),
                                   kurtosis(x))))
  # Create column names for matrix
  colnames(d) <- c("Min","10%","25%","Median 50%","75%", "90%", "Max",
                   "Mean", "Variance", "SD", "Skewness", "Kurtosis")
  
  if (isTRUE(transpose)){ t(d) } else { d } # Display
}
fin.stats(x = c("AMZN", "AAPL", "META", "GOOGL", "MSFT", "META", "NVDA",
                "TSLA"), lg = T, data = T, transpose = T, s = "2023-01-01")
