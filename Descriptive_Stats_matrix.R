stats_for_sec <- function(x){
  # Get logs of prices
  lrtn=diff(log(x))
  lrtn <- lrtn[-1,]
  
  # Calculate necessary statistics
  stats1 <- t(apply(lrtn,
                    2,
                    function(lrtn) c(mean(lrtn),
                                     quantile(lrtn,
                                              na.rm = T,
                                              probs = c(0.25)
                                     ),
                                     median(lrtn),
                                     quantile(lrtn,
                                              na.rm = T,
                                              probs = c(0.75)
                                     ),
                                     sd(lrtn),
                                     skewness(lrtn),
                                     kurtosis(lrtn)
                    )
  )
  )
  
  # Create colnames for matrix
  colnames(stats1) <- c("Mean",
                        "25%",
                        "50%",
                        "75%",
                        "SD",
                        "Skewness",
                        "Kurtosis")
  # Return output
  return(stats1)
  
}

stats_for_sec(portfolioReturns)
