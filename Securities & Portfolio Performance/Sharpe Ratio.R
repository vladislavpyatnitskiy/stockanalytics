# Script to calculate Sharpe Ratio

# Libraries 
library(quantmod)
library(timeSeries)

sharpe_ratio <- function(y, tr10 = "^TNX", start_date = NULL, end_date = NULL){
  
  # Add 10 year Treasuries to list
  y <- c(y, tr10)
  
  # Create an empty variable
  portfolioPrices <- NULL
  
  # Loop for data extraction
  for (Ticker in y){
    
    # Set up statements for start and end dates
    if (is.null(start_date) && is.null(end_date)) {
      
      # When neither start date nor end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = as.Date(Sys.Date()) - 365,
                                          to = Sys.Date(),
                                          src = "yahoo",
                                          auto.assign=FALSE)[,4]) } else { 
                                            
      # When both start date and end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker, from = start_date,
                                          to = end_date,
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4]) } }
  # Get rid of NAs
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!is.na(x))),]
  # Put the tickers in data set
  colnames(portfolioPrices) <- y
  
  # Make data discrete
  portfolioReturns <- ROC(portfolioPrices, type = "discrete")
  
  # Make it time series
  portfolioReturns <-as.timeSeries(portfolioPrices)
  
  # Calculate risk free return
  treasuries <- apply(portfolioReturns[,tr10],
                      2, function(col) mean(diff(log(col))))
  
  # Form matrix for other financial instruments
  security_prices <- portfolioReturns[,-which(names(portfolioReturns) == tr10)]
  
  # Turn to logs
  security_prices <- diff(log(security_prices))[-1,]
  
  # calculate mean and standard deviation
  params_clcd <- apply(security_prices, 2, function(x) c(mean(x), sd(x)))
  
  # Define new list name to contain Sharpe values
  df_sharpe <- NULL
  
  # Calculate Sharpe ratio
  for (k in 1:ncol(security_prices)){
    
    # Put values into list
    df_sharpe <- rbind(df_sharpe, (params_clcd[1,k]-treasuries)*(252^0.5) /
                         params_clcd[2,k]) }
  # Add tickers
  rownames(df_sharpe) <- tickers
  
  # Put name of ratio to column
  colnames(df_sharpe) <- "Sharpe"
  
  # Display values
  return(df_sharpe)
}
# Test
tickers <- c("STLA", "UNM", "NVDA", "AAPL", "CVNA")
sharpe_ratio(tickers, start_date = "2022-09-01")
