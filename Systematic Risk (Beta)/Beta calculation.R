# Libraries 
lapply(c("quantmod", "timeSeries"), require, character.only = TRUE)

# Function to generate beta values
c.beta <- function(y, bnchm = "^GSPC", s = NULL, e = NULL){
  
  y <- c(y, bnchm) # Add benchmark to list
  
  p <- NULL # Create an empty variable
  
  # Loop for data extraction & Set up statements for start and end dates
  for (Ticker in y){ if (is.null(s) && is.null(e)) {
    
      # When neither start date nor end date are defined
      p<-cbind(p,getSymbols(Ticker,from=as.Date(Sys.Date())-365,to=Sys.Date(),
                            src = "yahoo", auto.assign=F)[,4]) } else { 
                                 
      # When both start date and end date are defined
      p <- cbind(p, getSymbols(Ticker, from = s, to = e, src = "yahoo",
                               auto.assign=F)[,4]) } }
  
  p <- p[apply(p,1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  x=diff(log(as.timeSeries(p)))[-1,] # Calculate Returns and get rid of NA
  
  beta <- as.matrix(apply(x[, -which(names(x) == bnchm)], 2, # Calculate Beta
                    function(col) ((lm((col) ~ x[,bnchm]))$coefficients[2])))
  
  colnames(beta) <- "Beta" # Name column 
  
  return(beta) # Display values
}
# Test
c.beta(tickers_for_test, bnchm = "^GSPC")
