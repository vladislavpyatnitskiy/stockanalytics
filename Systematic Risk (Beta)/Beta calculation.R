lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries 

c.beta <- function(y, i = "^GSPC", beta=T, s=NULL, e=NULL){ # Beta or Alpha
  
  y <- c(y, i) # Add benchmark to list
  
  p <- NULL # Create an empty variable
  
  src <- "yahoo"
  
  getData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A, src=src, auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  for (A in y){ p <- cbind(p, getData(A, s, e)[,4]) } # Join data
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  x = diff(log(as.timeSeries(p)))[-1,] # Calculate Returns and get rid of NA
  
  c = ifelse(beta == T, 2, 1)
  
  B <- as.matrix(apply(x[, -which(names(x) == i)], 2,
                       function(col) ((lm((col) ~ x[,i]))$coefficients[c])))
  
  colnames(B) <- ifelse(beta == T, "Beta", "Alpha")
  
  return(B) # Display values
}
c.beta(tickers_for_test, i = "^GSPC", T) # Test
