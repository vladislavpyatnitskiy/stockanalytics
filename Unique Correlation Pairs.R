# Function to select unique correlation values
pairs_column <- function(x){
  
  # Calculate correlation matrix
  cor_matrix <- cor(x)
  
  # Extract unique pairs and their correlations
  cor_pairs <- which(upper.tri(cor_matrix, diag = TRUE), arr.ind = TRUE)
  
  # Put them into one data frame
  unique_pairs <- data.frame(
    Variable1 = rownames(cor_matrix)[cor_pairs[, 1]],
    Variable2 = rownames(cor_matrix)[cor_pairs[, 2]],
    Correlation = cor_matrix[cor_pairs]
  )
  
  # Filter out pairs with correlation equal to 1
  filtered_pairs <- unique_pairs[unique_pairs$Correlation != 1, ]
  
  # Print the filtered pairs
  return(filtered_pairs)
}
# Test
pairs_column(stock_data)
