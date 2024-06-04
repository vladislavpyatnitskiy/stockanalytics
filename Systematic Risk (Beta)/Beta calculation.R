# Libraries 
lapply(c("quantmod", "timeSeries"), require, character.only = TRUE)

c.beta <- function(y, i = "^GSPC", c=2, s = NULL, e = NULL){ # Beta || Alpha
  
  y <- c(y, i) # Add benchmark to list
  
  p <- NULL # Create an empty variable
  
  for (A in y){ if (is.null(s) && is.null(e)) { # When Time Period not defined
    
      p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign = F)[,4])
    
    } else if (is.null(e)) { # When only start date is defined
    
      p <- cbind(p,getSymbols(A, from = s, src = "yahoo", auto.assign = F)[,4])
    
    } else if (is.null(s)) { # When only end date is defined
    
      p <- cbind(p, getSymbols(A, to = e, src = "yahoo", auto.assign = F)[,4])
    
    } else { # When both start date and end date are defined
    
      p <- cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) } }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  x=diff(log(as.timeSeries(p)))[-1,] # Calculate Returns and get rid of NA
  
  beta <- as.matrix(apply(x[, -which(names(x) == i)], 2, # Calculate Beta
                          function(col) ((lm((col) ~ x[,i]))$coefficients[c])))
  
  if (c == 2){ colnames(beta) <- "Beta" } else { colnames(beta) <- "Alpha" }
  
  return(beta) # Display values
}
c.beta(tickers_for_test, i = "^GSPC", c = 2) # Test
