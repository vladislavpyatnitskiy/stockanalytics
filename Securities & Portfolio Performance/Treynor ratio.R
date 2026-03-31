lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Treynor script
treynor.ratio <- function(x, tr = "^TNX", i = "^GSPC", s = NULL, e=NULL){
  
  y <- c(x, tr, i) # Add 10 year Treasuries to list
  
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
  
  rf <- apply(p[,tr], 2, function(col) mean(col) / 100) # Risk Free Return
  
  # Calculate expected return for first row and beta on the second one
  r<-apply(
    diff(log(p[,1:length(x)]))[-1,], 2,
    function(col) c(
      exp(sum(col)) - 1,
      lm((col)~diff(log(p[,i]))[-1,])$coefficients[2]
      )
    )
  
  treynor <- NULL # Calculate Treynor values
  
  for (k in 1:length(x)){ treynor <- rbind(treynor, (r[1,k] - rf) / r[2,k]) }
  
  rownames(treynor) <- x # Add tickers
  colnames(treynor) <- "Treynor" # Put name of ratio to column
  
  return(treynor) # Display values
}
# Test
treynor.ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), s = "2022-09-01")
