cumulative.returns <- function(x){ # Calculate Cumulative Returns
  
  x <- diff(log(x)) # Calculate logs
  
  x[1,] <- 0 # Substitute "NA" with 0
  
  x <- apply(x, 2, function(col) exp(cumsum(col)) - 1) # Cumulative Returns
  
  x # Display
}
cumulative.returns(stock_data) # Test
