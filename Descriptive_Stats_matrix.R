# Descriptive Statistics
stat_for_assets <- function(x, lg = F){
  
  # Calculate logs and remove NA if needed
  if (isTRUE(lg)){ x = diff(log(x))[-1,] }
    
    # Calculate necessary statistics
    nec_stats <- t(apply(x, 2, function(x) c(min(x),
                                       quantile(x, na.rm = T, probs = c(0.1)),
                                       quantile(x, na.rm = T, probs = c(0.25)),
                                       median(x), # 50 %
                                       quantile(x, na.rm = T, probs = c(0.75)),
                                       quantile(x, na.rm = T, probs = c(0.9)),
                                       max(x),
                                       mean(x),
                                       var(x),
                                       sd(x),
                                       skewness(x),
                                       kurtosis(x)
                                       )
    )
    )
    
    # Create column names for matrix
    colnames(nec_stats) <- c("Min",
                          "10%",
                          "25%",
                          "Median 50%",
                          "75%",
                          "90%",
                          "Max",
                          "Mean",
                          "Variance",
                          "SD",
                          "Skewness",
                          "Kurtosis")
    # Return output
    return(nec_stats)
}

# Test
stat_for_assets(x = stock_data, lg = T)
