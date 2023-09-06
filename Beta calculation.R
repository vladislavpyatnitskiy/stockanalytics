# Function to generate beta values
beta_values <- function(x){
  
  # Calculate returns
  x=diff(log(x))[-1,]
  
  # Copy column S&P 500 column and make separate column
  spx <- x[,"^GSPC"]
  
  # Subset S&P 500 from data set
  stock_returns <- x[, -which(names(x) == "^GSPC")]
  
  # Calculate Betas
  x <- apply(stock_returns,
        2,
        function(col) ((lm((col) ~ spx))$coefficients[2]))
  
  # Transform into matrix 
  x <- as.matrix(x)
  
  # Name column 
  colnames(x) <- "Beta"
  
  # Display values
  return(x)
}
# Test             
beta_values(portfolioReturns)
