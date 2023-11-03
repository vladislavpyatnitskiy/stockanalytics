lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries 

# Betas with different indices
beta.indices <- function(y, benchmark = c("^GSPC"), period = 1){
  
  beta.df <- NULL # Create an empty variables for beta values
  
  for (m in 1:length(benchmark)){ b <- benchmark[m] # Set up variable
    
    l.beta <- c(y, b) # Add benchmark to list
    
    p <- NULL # Create an empty variables for security values
    
    for (Ticker in l.beta){ # Data upload
      
      p <- cbind(p, getSymbols(Ticker,from = as.Date(Sys.Date()) - 365*period,
                               to=Sys.Date(),src="yahoo",auto.assign=F)[,4]) } 
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- l.beta # Put the tickers in data set
    
    p=diff(log(as.timeSeries(p)))[-1,] # Time Series Returns and get rid of NA
    
    beta <- apply(p[, -which(names(p) == b)],2, # Calculate Betas
                  function(col) ((lm((col) ~ p[,b]))$coefficients[2]))
    
    # Put data into data frame
    if (is.null(beta.df)){ beta.df <- as.data.frame(beta) } else {
      
      beta.df <- cbind(beta.df, as.data.frame(beta)) } }
  
  colnames(beta.df) <- c(sprintf("Beta %s", benchmark)) # Give column names
  
  return(beta.df) # Display values
}
# Test
beta.indices(tickers_for_test,benchmark = c("^GSPC", "^IXIC", "^DJI", "^FTSE"),
             period = 1)
