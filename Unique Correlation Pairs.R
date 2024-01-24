pairs_column <- function(x, details = F){ # Unique Correlation values
  
  cor_matrix <- cor(x) # Calculate correlation matrix
  
  # Extract unique pairs and their correlations
  cor_pairs <- which(upper.tri(cor_matrix, diag = TRUE), arr.ind = TRUE)
  
  # Put them into one data frame
  unique_pairs <- data.frame(Variable1 = rownames(cor_matrix)[cor_pairs[, 1]],
                             Variable2 = rownames(cor_matrix)[cor_pairs[, 2]],
                             Correlation = cor_matrix[cor_pairs]
                             )
  # Filter out pairs with correlation equal to 1
  filtered_pairs <- unique_pairs[unique_pairs$Correlation != 1, ]
  
  if (isFALSE(details)){ rownames(filtered_pairs) <- seq(nrow(filtered_pairs))
  
    colnames(filtered_pairs) <- c("Security 1", "Security 2", "Correlation")
    
    return(filtered_pairs) } else { # Descriptive Statistics
      
      d <- apply(as.data.frame(filtered_pairs[,3]), 2,
                 function(x) c(min(x), median(x), max(x), mean(x)))
      
      rownames(d) <- c("Min","Median 50%","Max", "Mean") # Column names
      colnames(d) <- "Summary"
      
      d }
}
pairs_column(stock_data, details = F) # Test
