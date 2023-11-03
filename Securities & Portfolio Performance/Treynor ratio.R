# Libraries
lapply(c("quantmod", "timeSeries"), require, character.only = TRUE)

# Treynor script
treynor.ratio <- function(y, tr10 = "^TNX", spx = "^GSPC",s = NULL, e = NULL){
  
  y <- c(y, tr10, spx) # Add 10 year Treasuries to list
  
  p <- NULL # Create an empty variable
  
  for (Ticker in y){ if (is.null(s) && is.null(e)) { # Data upload
    
    # When neither start date nor end date are defined
    p<-cbind(p,getSymbols(Ticker,from=as.Date(Sys.Date())-365,to=Sys.Date(),
                          src="yahoo",auto.assign=F)[,4])
    
    } else if (is.null(e)) { # When only start date is defined
    
    p <- cbind(p, getSymbols(Ticker,from=s,src="yahoo",auto.assign=F)[,4])
    
    } else if (is.null(s)) { # When only end date is defined
    
    p <- cbind(p, getSymbols(Ticker,to=e,src = "yahoo",auto.assign=F)[,4])
    
    } else { # When both start date and end date are defined
    
    p<-cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])} }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  rf <- apply(p[,tr10],2,function(col) mean(diff(log(col)))) # Risk Free Return
  
  e.r<-apply(p[,1:(ncol(p)-2)],2,function(col) mean(diff(log(col)))) # Exp Ret
  
  b<-apply(diff(log(p[,1:(ncol(p)-2)]))[-1,], 2, # Beta
           function(col) ((lm((col)~diff(log(p[,spx]))[-1,]))$coefficients[2]))
  
  treynor <- NULL # Calculate Treynor values
  
  for (k in 1:(ncol(p)-2)){ treynor<-rbind(treynor,((e.r[k]-rf)/b[k])*252^.5)}
  
  rownames(treynor) <- tickers # Add tickers
  colnames(treynor) <- "Treynor" # Put name of ratio to column
  
  return(treynor) # Display values
}
# Test
treynor.ratio(c("STLA", "UNM", "NVDA", "AAPL", "CVNA"), s = "2022-09-01")
