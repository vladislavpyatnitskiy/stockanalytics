# Libraries
library(quantmod)
library(timeSeries)

# Treynor script
treynor_ratio <- function(y, z = NULL, i = NULL){
  # Define Yahoo ticker for 10 year Treasuries
  tr10 <- "^TNX"
  
  # Define Yahoo ticker for S&P 500
  spx <- "^GSPC"
  
  # Add 10 year Treasuries to list
  y <- c(y, tr10, spx)
  
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
  treasuries <- (portfolioReturns[,"^TNX"])
  
  # Calculate risk free return
  treasuries <- apply(treasuries,
                      2,
                      function(col) mean(diff(log(col))))
  
  # Form matrix for other financial instruments
  security_prices <- portfolioReturns[,1:(ncol(portfolioReturns)-2)]
 
  # Calculate expected return
  mean_calculated <- apply(security_prices,
                           2,
                           function(col) mean(diff(log(col))))
  
  # Subtract index from matrix
  spx_column <- (portfolioReturns[,"^GSPC"])
  
  # Calculate logs of securities
  security_prices=diff(log(security_prices))[-1,]
  
  # Calculate logs of S&P 500
  spx_column=diff(log(spx_column))[-1,]
  
  # Calculate beta
  beta_for_treynor <- apply(security_prices,
             2,
             function(col) ((lm((col) ~ spx_column))$coefficients[2]))
  
  # Define new list name to contain Treynor values
  df_treynor <- NULL
  
  # Calculate Treynor ratio
  for (k in 1:(ncol(security_prices))){
    final_treynor_coef <- ((mean_calculated[k] - treasuries) /
                            beta_for_treynor[k]) * (252^0.5)
    
    # Put values into list
    df_treynor <- cbind(df_treynor, final_treynor_coef)
  }
  
  # Add tickers
  colnames(df_treynor) <- tickers
  
  # Put name of ratio to column
  rownames(df_treynor) <- "Treynor"
  
  # Transpose
  df_treynor <- t(df_treynor)
  
  # Display values
  return(df_treynor)
}

tickers <- c("STLA", "UNM", "NVDA", "AAPL", "CVNA")
treynor_ratio(tickers, "2022-09-01")
