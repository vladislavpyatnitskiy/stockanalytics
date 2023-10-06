# Function for Shapiro Test
test_for_shapiro <- function(x, lg = F){
  
  #Calculate logs and remove NA if needed
  if (isTRUE(lg)){ x = diff(log(x))[-1,] }
  
  # Create place to store values
  table_for_shapiro <- NULL
  
  # For each column find values of Shapiro and store them into matrix
  for (n in 1:ncol(x)){ table_for_shapiro <- cbind(table_for_shapiro,
                                                   shapiro.test(x[,n])) }
  # Give it column names from original matrix
  colnames(table_for_shapiro) <- colnames(x)
  
  # Get rid of excess rows
  table_for_shapiro <- table_for_shapiro[1:(nrow(table_for_shapiro)-2),]
  
  # Display values
  return(table_for_shapiro)
}
# Test
test_for_shapiro(portfolioReturns, lg = T)
