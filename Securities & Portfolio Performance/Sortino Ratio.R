lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libraries

# Function to calculate Sortino Ratio
sortino.ratio <- function(y,tr = "^TNX",s = as.Date(Sys.Date())-365,
                          e = as.Date(Sys.Date()),v.sortino = NULL){
  
  y <- c(y, tr) # Add 10 year Treasuries to list
  
  p <- NULL # Create list with securities data
  
  for (A in y){ p <- cbind(p, getSymbols(A, from = s, to = e, src = "yahoo",
                                         auto.assign=F)[,4]) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  p <- diff(log(as.timeSeries(p)))[-1,] # Clean data for returns
  
  rf <- apply(p[,tr], 2, function(col) mean(col)) # Risk Free Rate
  
  r <- p[, -which(names(p) == tr)] # Matrix with financial instruments
  
  r.mean <- apply(r, 2, function(col) mean(col)) # Calculate return
  
  S <- NULL # List for calculated below Sortino values 
  
  for (k in 1:ncol(r)){ excess.r <- r[,k] - r.mean[k] # Excess Return
  
    S <- rbind(S, (mean(excess.r)-rf) / mean((excess.r[excess.r < 0])^2)^.5) } 
  
  rownames(S) <- colnames(r) # Add tickers
  colnames(S) <- "Sortino" # Put name of ratio to column
  
  return(S) # Display values
}
# Test
sortino.ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), s = "2022-09-01")
