beta.yahoo <- function(x){ # Function to get info about company beta
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s", x, x)
  
  page.p <- read_html(p) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
  
  yahoo.header1 <- tab11 %>% html_nodes('tr')%>%html_nodes('td')%>%html_text()
  
  b <- yahoo.header1[grep("Beta ", yahoo.header1) + 1] # Scrape Beta value
  
  names(b) <- sprintf("Beta %s", x) # Assign name for beta with ticker
  
  b # Display
}
beta.yahoo("M") # Test
