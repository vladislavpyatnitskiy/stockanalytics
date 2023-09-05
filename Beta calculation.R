beta_values <- function(x){
  x=diff(log(x))
  x <- x[-1,]
  
  market_return <- x$`^GSPC`
  market_return
  
  stock_returns <- x[,1:(ncol(x) - 1)]
  stock_returns
  
  x <- apply(stock_returns,
        2,
        function(col) ((lm((col) ~ market_return))$coefficients[2]))
  x <- t(x)
}
View(beta_values(portfolioReturns))
