lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries 

# Betas with different indices
beta.indices <- function(y, benchmark = c("^GSPC"), period = 1){
  
  df <- NULL # Select data for Beta
  
  for (m in 1:length(benchmark)){ l.beta <- c(y, benchmark[m])
  
    p <- NULL # Create an empty variables for security values
    
    for (Ticker in l.beta){ # Data upload
      
      p <- cbind(p, getSymbols(Ticker,from = as.Date(Sys.Date()) - 365*period,
                               to=Sys.Date(),src="yahoo",auto.assign=F)[,4]) } 
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- l.beta # Put the tickers in data set
    
    p=diff(log(as.timeSeries(p)))[-1,] # Time Series Returns and get rid of NA
    
    b <- apply(p[, -which(names(p) == benchmark[m])],2, # Calculate Betas
               function(col) ((lm((col) ~ p[,benchmark[m]]))$coefficients[2]))
    
    if (is.null(df)){ df <- as.data.frame(b) } else {
      
      df <- cbind(df, as.data.frame(b)) } } # Join data
    
  colnames(df) <- c(sprintf("Beta %s", benchmark)) # Give column names
  
  return(df) # Display values
}
# Test
beta.indices(tickers_for_test,benchmark = c("^GSPC", "^IXIC", "^DJI", "^FTSE"),
             period = 1)
