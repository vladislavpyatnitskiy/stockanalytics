cmlrets <- function(x){ # Calculate Cumulative Returns
  
  apply(diff(log(x))[-1,], 2, function(col) exp(cumsum(col)) - 1) 
}
cmlrets(stock_data) # Test
