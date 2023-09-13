stat_for_assets <- function(x){
    # Calculate log returns and remove NA
    x=diff(log(x))[-1,]
    
    # Calculate necessary statistics
    nec_stats <- t(apply(x,
                      2,
                      function(x) c(min(x),
                                       quantile(x,
                                                na.rm = T,
                                                probs = c(0.1)
                                       ),
                                       quantile(x,
                                                na.rm = T,
                                                probs = c(0.25)
                                       ),
                                       median(x),
                                       quantile(x,
                                                na.rm = T,
                                                probs = c(0.75)
                                       ),
                                       quantile(x,
                                                na.rm = T,
                                                probs = c(0.9)
                                       ),
                                       max(x),
                                       mean(x),
                                       sd(x),
                                       skewness(x),
                                       kurtosis(x)
                      )
    )
    )
    
    # Create colnames for matrix
    colnames(nec_stats) <- c("Min",
                          "10%",
                          "25%",
                          "50%",
                          "75%",
                          "90%",
                          "Max",
                          "Mean",
                          "SD",
                          "Skewness",
                          "Kurtosis")
    # Return output
    return(nec_stats)
}
# Test
stat_for_assets(stock_data)
