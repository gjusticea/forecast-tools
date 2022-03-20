library(tidyverse)
library(quantmod)

runMC = function(symbol,thold,exp,trials=10000,strt=as.character(Sys.Date()-365),shiftsizeThold=.15){
  require(tidyverse)
  require(quantmod)
  
  expiration = as.Date(exp)
  
  #1/2 chance of sign flip
  u = function() return(sample(c(-1,1),1))
  
  #pull the most recent data on FB from Yahoo via quantmod package
  getSymbols(symbol,from=strt,warnings = FALSE,auto.assign = TRUE)
  phist = get(symbol)
  phist = phist[c(1:(nrow(phist)-1))]
  colnames(phist) = c("Open","High","Low","Close","Volume","Adjusted")
  
  #see what the most recent day with available data is (data for the current day isn't always available)
  mostRecentDate = max(as.Date(rownames(as.data.frame(phist))))
  phist = phist[which(!is.na(phist$Close)),]
  
  current_price = as.numeric(phist[nrow(phist),"Close"])
  
  remaining_days = as.numeric(as.Date(expiration) - mostRecentDate)
  trials = 10000
  curr_price_weight = 0.2
  sim_price_weight = 1-curr_price_weight
  
  # Get a set of price shifts
  
  df = as.data.frame(phist$Close)
  df$Previous = c(NA,df[c(1:(nrow(df)-1)),"Close"])
  df = df[which(!is.na(df$Previous)),]
  df$dte = as.Date(rownames(df))
  shifts = (df$Close/df$Previous)-1
  shifts = shifts[abs(shifts) < shiftsizeThold]
  
  aboveCnt = 0
  for(j in 1:trials){
    above = FALSE
    price = current_price
    for(i in 1:remaining_days){
      move = sample(shifts,1) * (curr_price_weight * current_price + sim_price_weight * price) * u()
      price = price + move
      if(price >= thold){
        above = TRUE
      }
    }
    if(above == TRUE){aboveCnt = aboveCnt + 1}
  }
  
  print(floor((aboveCnt/trials)*100))
}

runMC("FB",238,"2022-05-20")


