# Calculate Betas for different periods
beta_df <- function(y, bnchm = "^GSPC", period = 1){
  
  # Add benchmark to list
  y <- c(y, bnchm)
  
  # Create an empty variables for beta values
  beta_list <- NULL
  
  # Set up period for betas
  beta_dates <- c(as.Date(Sys.Date()) - 365 * period)
  
  # For each time period
  for (m in 1:length(beta_dates)){
    
    # Create an empty variables for security values
    portfolioPrices <- NULL
    
    # Loop for data extraction
    for (Ticker in y){
      
      # When neither start date nor end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = beta_dates[m],
                                          to = Sys.Date(),
                                          src = "yahoo",
                                          auto.assign=FALSE)[,4]) } 
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
    beta <- apply(stock_returns,2,function(col) ((lm((col) ~
                                                       spx))$coefficients[2]))
    # Contain one year Beta column
    if (is.null(beta_list)){ beta_list <- as.data.frame(beta) } else {
      
      # Add five year Beta column
      beta_list <- cbind(beta_list, as.data.frame(beta)) } }
  
  # Give column names
  colnames(beta_list) <- c(sprintf("Beta %sY", period))
  
  # Display values
  return(beta_list)
}
# Test
beta_df(tickers_for_test, bnchm = "^GSPC", period = c(1,5,10))
