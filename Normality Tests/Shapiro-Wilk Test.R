# Shapiro Test
t.shapiro <- function(x, lg = F, table = NULL){
  
  # Calculate logs and remove NA if needed
  if (isTRUE(lg)){ x = diff(log(x))[-1,] }
  
  # For each column find values of Shapiro and store them into matrix
  for (n in 1:ncol(x)){ table <- cbind(table, shapiro.test(x[,n])) }
  
    # Give it column names from original matrix
    colnames(table) <- colnames(x)
    
    # Display values
    return(table[1:(nrow(table)-2),])
}
t.shapiro(stock_data, lg = T) # Test
