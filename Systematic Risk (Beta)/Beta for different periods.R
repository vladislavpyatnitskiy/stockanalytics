# Calculate Betas for different periods

lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

beta.ratios <- function(y, bnchm = "^GSPC", period = 1){
  
  y <- c(y, bnchm) # Add benchmark to list
  
  l.beta <- NULL # Create an empty variables for beta values
  
  d.beta <- c(as.Date(Sys.Date()) - 365 * period) # Set up period for betas
  
  for (m in 1:length(d.beta)){ # For each time period
    
    p <- NULL # Empty variables for security values & Data upload
    
    for (S in y){ p<-cbind(p,getSymbols(S,from=d.beta[m],to=Sys.Date(),
                                        src="yahoo", auto.assign=F)[,4]) } 
  
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- y # Put the tickers in data set
    
    p=diff(log(as.timeSeries(p)))[-1,] # Time Series Returns and NA off
    
    beta <- apply(p[, -which(names(p) == bnchm)],2, # Beta
                  function(col) ((lm((col) ~ p[,bnchm]))$coefficients[2]))
    
    # Join Beta values into data frame
    if (is.null(l.beta)){ l.beta <- as.data.frame(beta) } else {
      
      l.beta <- cbind(l.beta, as.data.frame(beta)) } }
  
  colnames(l.beta) <- c(sprintf("Beta %sY", period)) # Give column names
  
  return(l.beta) # Display values
}
# Test
beta.ratios(tickers_for_test, bnchm = "^GSPC", period = c(1,5,10))
