# Function to generate beta values
beta_values <- function(y, bnchm = "^GSPC",
                        start_date = NULL, end_date = NULL){
  
  # Add benchmark to list
  y <- c(y, bnchm)
  
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
                                          auto.assign=FALSE)[,4])
    } else { 
      # When both start date and end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = start_date,
                                          to = end_date,
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4]) }
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
  
  # Calculate Returns and get rid of NA
  x=diff(log(portfolioReturns))[-1,]
  
  # Copy column index column and make separate column
  spx <- x[,bnchm]
  
  # Subset index from data set
  stock_returns <- x[, -which(names(x) == bnchm)]
  
  # Calculate Betas
  beta <- apply(stock_returns,
                  2,
                  function(col) ((lm((col) ~ spx))$coefficients[2]))
  
  # Transform into matrix 
  beta <- as.matrix(beta)
  
  # Name column 
  colnames(beta) <- "Beta"
  
  # Display values
  return(beta)
}
# Test
beta_values(tickers_for_test, bnchm = "^GSPC")
