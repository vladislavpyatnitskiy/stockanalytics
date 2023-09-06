# Create function
test_for_shapiro <- function(x){
  
  # put column names for future matrix
  columns_for_shapiro <- colnames(x)
  
  # Create place to store values
  table_for_shapiro <- NULL
  
  # For each column in matrix
  for (n in 1:ncol(x)){
    
    # Find values of Shapiro
    value_for_shapiro <- shapiro.test(x[,n])
    
    # Store them into matrix
    table_for_shapiro <- cbind(table_for_shapiro, value_for_shapiro)
  }
  # Give it column names from original matrix
  colnames(table_for_shapiro) <- columns_for_shapiro
  
  # Get rid of excess rows
  table_for_shapiro <- table_for_shapiro[1:(nrow(table_for_shapiro)-2),]
  
  # Display values
  return(table_for_shapiro)
}
# Test it
test_for_shapiro(lrtn)
