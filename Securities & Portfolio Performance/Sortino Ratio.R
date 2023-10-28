# Libraries
library(quantmod)
library(timeSeries)

# Function to calculate Sortino Ratio
sortino.ratio <- function(y,tr10 = "^TNX",z = NULL,i = NULL,v.sortino = NULL){
  
  y <- c(y, tr10) # Add 10 year Treasuries to list
  
  portfolioPrices <- NULL # Create an empty variable
  
  # Loop for data extraction and set up statements for start and end date
  for (Ticker in y){ if (is.null(start_date) && is.null(end_date)) {
      
      # When neither start date nor end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = as.Date(Sys.Date()) - 365,
                                          to = Sys.Date(),
                                          src = "yahoo",
                                          auto.assign=FALSE)[,4]) } else { 
                                            
      # When both start date and end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker, 
                                          from = start_date,
                                          to = end_date,
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4]) } }
  # Get rid of NAs
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!is.na(x))),]
  
  colnames(portfolioPrices) <- y # Put the tickers in data set
  
  r <- ROC(portfolioPrices, type = "discrete") # Make data discrete
  
  r <- diff(log(as.timeSeries(portfolioPrices)))[-1,] # Clean data for returns
  
  rf <- apply(r[,tr10], 2, function(col) mean(col)) # Risk Free Rate
  
  s <- r[, -which(names(r) == tr10)] # Matrix with financial instruments
  
  r.mean <- apply(s, 2, function(col) mean(col)) # Calculate return
  
  # Calculate Sortino ratio and calculate excess returns
  for (k in 1:ncol(s)){ r.premium <- s[,k] - r.mean[k]
    
    e.r <- mean(s[,k] - r.mean[k]) # Mean of Excessive returns
    
    n.r <- (r.premium[r.premium < 0]) ^ 2 # Negative excessive returns and ^2
    
    # Calculate Downside Risk, Sortino ratio and add to list
    v.sortino<-rbind(v.sortino, ((e.r-rf)/(sum(n.r)/nrow(s))^(0.5))*252^0.5) }
    
  rownames(v.sortino) <- colnames(s) # Add tickers
  
  colnames(v.sortino) <- "Sortino" # Put name of ratio to column
  
  return(v.sortino) # Display values
}
# Test
sortino.ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), z = "2022-09-01")
