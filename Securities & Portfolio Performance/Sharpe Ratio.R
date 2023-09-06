# Script to calculate Sharpe Ratio

# Libraries 
library(quantmod)
library(timeSeries)

sharpe_ratio <- function(y, z = NULL, i = NULL){
  # Define Yahoo ticker for 10 year Treasuries
  tr10 <- "^TNX"
    
  # Add 10 year Treasuries to list
  y <- c(y, tr10)
    
  # Create an empty variable
  portfolioPrices <- NULL
    
  # Loop for data extraction
  for (Ticker in y){
    # Set up statements for start and end dates
    if (is.null(z) && is.null(i)) {
      # When neither start date nor end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                                getSymbols(Ticker,
                                          from = as.Date(Sys.Date()) - 365,
                                          to = Sys.Date(),
                                          src = "yahoo",
                                          auto.assign=FALSE)[,4])
    } else if (is.null(i)) {
      # When only start date is defined
      portfolioPrices <- cbind(portfolioPrices,
                                getSymbols(Ticker, from = z, src = "yahoo",
                                          auto.assign=FALSE)[,4])
    } else if (is.null(z)) {
      # When only end date is defined
      portfolioPrices <- cbind(portfolioPrices,
                                getSymbols(Ticker, to = i, src = "yahoo",
                                          auto.assign=FALSE)[,4])
    } else { 
      # When both start date and end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                                getSymbols(Ticker, from = z, to = i,
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4])
      }
  }
  # Get rid of NAs
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                             function(x) all(!is.na(x))),]
  # Put the tickers in data set
  colnames(portfolioPrices) <- y
    
  # Make data discrete
  portfolioReturns <- ROC(portfolioPrices, type = "discrete")
    
  # Make it time series
  portfolioReturns <-as.timeSeries(portfolioPrices)
  
  # Subtract treasuries from matrix
  treasuries <- (portfolioReturns[,ncol(portfolioReturns)])
  
  # Calculate risk free return
  treasuries <- apply(treasuries,
                      2,
                      function(col) mean(diff(log(col))))
  
  # Form matrix for other financial instruments
  security_prices <- portfolioReturns[,1:(ncol(portfolioReturns)-1)]
  
  # Calculate Standard Deviation
  sd_calculated <- apply(security_prices,
                         2,
                         function(col) sd(diff(log(col))))
  
  # Calculate return
  mean_calculated <- apply(security_prices,
                           2,
                           function(col) mean(diff(log(col))))
  
  # Define new list name to contain Sharpe values
  df_sharpe <- NULL
  
  # Calculate Sharpe ratio
  for (k in 1:(ncol(security_prices))){
    final_sharpe_coef <- ((mean_calculated[k] - treasuries) /
      sd_calculated[k]) * (252^0.5)
    
    # Put values into list
    df_sharpe <- cbind(df_sharpe, final_sharpe_coef)
  }
  
  # Add tickers
  colnames(df_sharpe) <- tickers
  
  # Put name of ratio to column
  rownames(df_sharpe) <- "Sharpe"
  
  # Transpose
  df_sharpe <- t(df_sharpe)
  
  # Display values
  return(df_sharpe)
}
# Test 
tickers <- c("STLA", "UNM", "NVDA", "AAPL", "CVNA")
sharpe_ratio(tickers, "2022-09-01")
