# Betas with different indices
df_with_betas <- function(y, bnchm = c("^GSPC"), period = 1){
  
  # Create an empty variables for beta values
  beta_list <- NULL
  
  # For each benchmark
  for (m in 1:length(bnchm)){
    
    # Set up variable 
    bnchm_for_loop <- bnchm[m]
    
    # Add benchmark to list
    list_y <- c(y, bnchm_for_loop)
    
    # Create an empty variables for security values
    portfolioPrices <- NULL
    
    # Loop for data extraction
    for (Ticker in list_y){
      
      # When neither start date nor end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                               getSymbols(Ticker,
                                          from = as.Date(Sys.Date()) - 365 *
                                            period,
                                          to = Sys.Date(),
                                          src = "yahoo",
                                          auto.assign=FALSE)[,4]) } 
    # Get rid of NAs
    portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                             function(x) all(!is.na(x))),]
    # Put the tickers in data set
    colnames(portfolioPrices) <- list_y
    
    # Make data discrete
    portfolioReturns <- ROC(portfolioPrices, type = "discrete")
    
    # Make it time series
    portfolioReturns <-as.timeSeries(portfolioPrices)
    
    # Calculate Returns and get rid of NA
    x=diff(log(portfolioReturns))[-1,]
    
    # Copy column index column and make separate column
    spx <- x[,bnchm_for_loop]
    
    # Subset index from data set
    stock_returns <- x[, -which(names(x) == bnchm_for_loop)]
    
    # Calculate Betas
    beta <- apply(stock_returns,2,function(col) ((lm((col) ~
                                                       spx))$coefficients[2]))
    # Contain one year Beta column
    if (is.null(beta_list)){ beta_list <- as.data.frame(beta) } else {
      
      # Add five year Beta column
      beta_list <- cbind(beta_list, as.data.frame(beta)) } }
  
  # Give column names
  colnames(beta_list) <- c(sprintf("Beta %s", bnchm))
  
  # Display values
  return(beta_list)
}
# Test
df_with_betas(tickers_for_test, bnchm = c("^GSPC", "^IXIC", "^DJI", "^FTSE"),
              period = 1)
