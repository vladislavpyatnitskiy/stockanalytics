lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

MAE <- function(x, s = NULL, e = NULL, data = T){ # Mean Absoulte Error
  
  if (isTRUE(data)){ p <- NULL # Create an empty variable
    
    # Loop for data extraction & # Set up statements for start and end dates
    for (A in x){ if (is.null(s) && is.null(e)) {
      
      # When neither start date nor end date are defined
      p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign=F)[,4])
      
    } else if (is.null(e)) { # When only start date is defined
      
      p <- cbind(p, getSymbols(A, from = s,src="yahoo",auto.assign=F)[,4])
      
    } else if (is.null(s)) { # When only end date is defined
      
      p <- cbind(p,getSymbols(A, to = e, src = "yahoo", auto.assign = F)[,4])
      
    } else { # When both start date and end date are defined
      
      p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4])}
    }
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in data set
    
    x <- as.timeSeries(p) } # Make it time series
    
  l <- NULL
  
  for (n in 1:ncol(x)){ s <- x[,n]
    
    l <- rbind(l, sum(abs(s - mean(s))) / length(s)) }

  rownames(l) <- colnames(x)
  colnames(l) <- "MAE"
  l
}
MAE(c("AAPL", "C"), s = "2020-01-01", data = T) # Test
