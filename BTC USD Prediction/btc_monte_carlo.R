# Credit to Jonathan Mann for his monte carlo script at https://github.com/jonathanmann/gjopen/blob/master/bitcoin_price_forecasting/btc_monte_carlo.py

library(tidyverse)
library(quantmod)

#1/3 chance of sign flip
u = function() return(sample(c(-1,1,1),1))

#pull the most recent data on BTC-USD from Yahoo via quantmod package
getSymbols("BTC-USD",from="2021-01-01",to=as.character(Sys.Date()),warnings = FALSE,auto.assign = TRUE)

#see what the most recent day with available data is (data for the current day isn't always available)
mostRecentDate = max(as.Date(rownames(as.data.frame(`BTC-USD`))))
`BTC-USD` = `BTC-USD`[which(!is.na(`BTC-USD`$`BTC-USD.Close`)),]

expiration = as.Date("2021-07-01")
current_price = as.numeric(`BTC-USD`[nrow(`BTC-USD`),"BTC-USD.Close"])
remaining_days = as.numeric(expiration - mostRecentDate)
trials = 10000
curr_price_weight = 0.2
sim_price_weight = 1-curr_price_weight

df = as.data.frame(`BTC-USD`$`BTC-USD.Close`)
df$Previous = c(NA,df[c(1:(nrow(df)-1)),"BTC-USD.Close"])
df = df[which(!is.na(df$Previous)),]
df$dte = as.Date(rownames(df))
shifts = (df$`BTC-USD.Close`/df$Previous)-1

low = 0
high = 0


for(j in 1:trials){
  price = current_price
  for(i in 1:remaining_days){
    move = sample(shifts,1) * (curr_price_weight * current_price + sim_price_weight * price) * u()
    price = price + move
    if(price < 25000){
      low = low+1
      break
    }
    if(price > 100000){
      high = high+1
      break
    }
  }
}

print(paste0("Low: ",round((low/trials)*100,1),"%, High: ",round(high/trials,1),"%, Neither: ",round(((trials-high-low)/trials)*100,1),"%"))



# Get historic rate of price fluctuations as large as the one needed for a sub-$25k resolution (prediction should probably be in this ballpark)

changeLevelLow = 25000/current_price
changeLevelHigh = 2-changeLevelLow

getFluctuationYN = function(dte,price){
  tmp = df[which(df$dte %in% seq.Date(from = dte,to = dte+remaining_days,by="days")),"BTC-USD.Close"]
  checkHigh = price*changeLevelHigh
  checkLow = price*changeLevelLow
  
  out = FALSE
  # if(min(tmp) >= checkHigh) out = TRUE
  if(min(tmp) <= checkLow) out = TRUE
  
  return(out)
}
df$fluct = unlist(mapply(FUN = getFluctuationYN,df$dte,df$`BTC-USD.Close`))
df2 = df[which(df$dte < (mostRecentDate - remaining_days)),]

print(paste0("Dip chance (low): ",round(length(which(df$fluct == TRUE))/nrow(df),3)*100,"%"))
print(paste0("Dip chance (high): ",round(length(which(df2$fluct == TRUE))/nrow(df2),3)*100,"%"))


# Test brier scores with different current price weights (sample with large fluctuations too small, results overfitted)

u = function(thold=0.3333){
  tmp = runif(1)
  out = 1
  if(tmp >= thold) out = -1
  return(out)
}

getNeitherPrediction = function(startDate,curr_price_weight=0.2,thold = 0.3333){
  expiration = startDate + remaining_days
  current_price = as.numeric(`BTC-USD`[which(as.Date(rownames(as.data.frame(`BTC-USD`))) == startDate),"BTC-USD.Close"])
  sim_price_weight = 1-curr_price_weight
  
  dfTemp = df[which(as.Date(rownames(df)) <= startDate),]
  shifts = (dfTemp$`BTC-USD.Close`/dfTemp$Previous)-1
  
  low = 0
  high = 0
  
  for(j in 1:trials){
    price = current_price
    for(i in 1:remaining_days){
      move = sample(shifts,1) * (curr_price_weight * current_price + sim_price_weight * price) * u(thold)
      price = price + move
      if(price < current_price*changeLevelLow){
        low = low+1
        break
      }
      if(price > current_price*changeLevelHigh){
        high = high+1
        break
      }
    }
  }
  return((trials-low)/trials)
}

testFrame = data.frame(dte = seq.Date(from = as.Date("2021-02-14"),to = (mostRecentDate - remaining_days),by="days"))
testFrame$dayPrice = unlist(lapply(testFrame$dte,FUN = function(x) df[which(df$dte == x),"BTC-USD.Close"]))

for(i in seq(0.05,0.5,0.05)){
  testFrame[,paste0("trialWeight",i)] = unlist(lapply(testFrame$dte,FUN = getNeitherPrediction,curr_price_weight = i))
}
for(i in seq(0.05,0.5,0.05)){
  testFrame[,paste0("trialFlip",i)] = unlist(lapply(testFrame$dte,FUN = getNeitherPrediction,thold = i))
}

testFrame$weightDiff = testFrame$trialWeight0.5 - testFrame$trialWeight0.05
testFrame$fluct = unlist(mapply(FUN = getFluctuationYN,testFrame$dte,testFrame$dayPrice))
testFrame[which(testFrame$fluct == FALSE),"fluct2"] = 1
testFrame[which(testFrame$fluct == TRUE),"fluct2"] = 0

for(i in seq(0.05,0.5,0.05)){
  out = mean(unlist(mapply(FUN = function(x,y) (y-x)^2,testFrame$fluct2,testFrame[,paste0("trialFlip",i)])))
  print(paste0("Flip Chance of ",i,": ",out))
}

sum(testFrame$fluct2)
