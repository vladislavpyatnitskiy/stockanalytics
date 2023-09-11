# Libraries
library(quantmod)
library(timeSeries)

# Function to calculate Sortino Ratio
sortino_ratio <- function(y, z = NULL, i = NULL){
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
  
  # Make logs and get rid of NA
  portfolioReturns <- diff(log(portfolioReturns))[-1,]
  
  # Subtract treasuries from matrix
  treasuries <- (portfolioReturns[,ncol(portfolioReturns)])
  
  # Calculate risk free return
  treasuries <- apply(treasuries,
                      2,
                      function(col) mean(col))
  
  # Form matrix for other financial instruments
  security_prices <- portfolioReturns[,1:(ncol(portfolioReturns)-1)]
  
  # Column names for Sortino
  sortino_names <- colnames(security_prices)
  
  # Calculate return
  mean_calculated <- apply(security_prices,
                           2,
                           function(col) mean(col))
  
  # Define new list name to contain Sharpe values
  df_sortino <- NULL
  
  # Calculate Sortino ratio
  for (k in 1:(ncol(security_prices))){
    
    # Calculate excess returns
    excess_ret_calculated <- security_prices[,k] - mean_calculated[k]
    
    # Mean of Excessive returns
    excess_ret_calculated_mean <- mean(excess_ret_calculated)
    
    # Find negative excessive returns
    Neg_exc_ret <- excess_ret_calculated[excess_ret_calculated < 0]
    
    # Square negative excessive returns
    Neg_exc_ret <- ((Neg_exc_ret)^2)
    
    # Calculate Downside Risk
    downside_risk <- ((sum(Neg_exc_ret))/(nrow(security_prices)))^(0.5)
    
    # Final calculation
    final_sortino_coef <- ((excess_ret_calculated_mean - treasuries) /
                            downside_risk) * (252^0.5)
    
    # Put values into list
    df_sortino <- cbind(df_sortino, final_sortino_coef)
  }
  
  # Add tickers
  colnames(df_sortino) <- sortino_names
  
  # Put name of ratio to column
  rownames(df_sortino) <- "Sortino"
  
  # Transpose
  df_sortino <- t(df_sortino)
  
  # Display values
  return(df_sortino)
}
# Test
tickers <- c("STLA", "UNM", "NVDA", "AAPL", "CVNA")
sortino_ratio(tickers, "2022-09-01")
