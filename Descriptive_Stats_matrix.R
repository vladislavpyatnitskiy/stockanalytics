lapply(c("quantmod","timeSeries","fBasics"),require,character.only=T) # Libs

fin.stats <- function(x, lg = F, data = F, transpose = F, s = NULL, e = NULL){ 
  
  if (data){ p <- NULL
    
    src="yahoo"
  
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
  
  if (lg) x = diff(log(x))[-1,] # logs & remove NA if needed
  
  # Calculate necessary statistics
  d <- t(
    apply(x, 2, function(x) c(
      min(x),
      quantile(x, na.rm=T, probs = c(.1)),
      quantile(x, na.rm=T, probs = c(.25)),
      median(x),
      quantile(x, na.rm=T, probs = c(.75)),
      quantile(x, na.rm=T, probs = c(.9)),
      max(x),
      mean(x),
      var(x),
      sd(x),
      skewness(x),
      kurtosis(x)
      )
      )
    )
  
  # Create column names for matrix
  colnames(d) <- c(
    "Min", "10%", "25%", "Median 50%", "75%", "90%", "Max", "Mean", "Variance",
    "SD", "Skewness", "Kurtosis")
  
  if (transpose){ t(d) } else { d } # Display
}
fin.stats(x = c("AMZN", "AAPL", "META", "GOOGL", "MSFT", "META", "NVDA",
                "TSLA"), lg = T, data = T, transpose = T, s = "2023-01-01")
