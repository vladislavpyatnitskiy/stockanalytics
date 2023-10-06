# Libraries
library(quantmod)
library(timeSeries)

# Treynor script
treynor_ratio <- function(y, tr10 = "^TNX", spx = "^GSPC",
                          start_date = NULL, end_date = NULL){
  
  # Add 10 year Treasuries to list
  y <- c(y, tr10, spx)
  
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
  treasuries <- apply(portfolioReturns[,tr10], 2,
                      function(col) mean(diff(log(col))))
  
  # Form matrix for other financial instruments
  security_prices <- portfolioReturns[,1:(ncol(portfolioReturns)-2)]
 
  # Calculate expected return
  mean_calculated <- apply(security_prices, 2,
                           function(col) mean(diff(log(col))))
  
  # Subtract index from matrix
  spx_column <- portfolioReturns[,spx]
  
  # Calculate logs of securities
  security_prices=diff(log(security_prices))[-1,]
  
  # Calculate logs of S&P 500
  spx_column=diff(log(spx_column))[-1,]
  
  # Calculate beta
  beta_treynor <- apply(security_prices,2,function(col) ((lm((col) ~
                                  spx_column))$coefficients[2]))
  
  # Define new list name to contain Treynor values
  df_treynor <- NULL
  
  # Calculate Treynor ratio
  for (k in 1:ncol(security_prices)){ 
    df_treynor <- rbind(df_treynor,((mean_calculated[k]- treasuries) /
                                      beta_treynor[k])* 252^0.5) }
  # Add tickers
  rownames(df_treynor) <- tickers
  
  # Put name of ratio to column
  colnames(df_treynor) <- "Treynor"
  
  # Display values
  return(df_treynor)
}
# Test
tickers <- c("STLA", "UNM", "NVDA", "AAPL", "CVNA")
treynor_ratio(tickers, start_date = "2022-09-01")
