library(tidyverse)
library(quantmod)

#1/3 chance of sign flip
u = function() return(sample(c(-1,1,1),1))

#pull the most recent data on UBER from Yahoo via quantmod package
getSymbols("UBER",from="2019-05-11",to=as.character(Sys.Date()),warnings = FALSE,auto.assign = TRUE)

#see what the most recent day with available data is (data for the current day isn't always available)
mostRecentDate = max(as.Date(rownames(as.data.frame(UBER))))
UBER = UBER[which(!is.na(UBER$`UBER.Close`)),]

expiration = as.Date("2021-12-31")
current_price = as.numeric(UBER[nrow(UBER),"UBER.Close"])
remaining_days = as.numeric(expiration - mostRecentDate)
trials = 10000
curr_price_weight = 0.2
sim_price_weight = 1-curr_price_weight

# Get a set of price shifts

df = as.data.frame(UBER$`UBER.Close`)
df$Previous = c(NA,df[c(1:(nrow(df)-1)),"UBER.Close"])
df = df[which(!is.na(df$Previous)),]
df$dte = as.Date(rownames(df))
shifts = (df$`UBER.Close`/df$Previous)-1
# Cut out the large shifts around COVID and the CA referendum, I'm looking for moving volatility with this script
shifts = shifts[which(abs(shifts) <= .15)]

sharesOutstanding = 1.87
sub75 = 0
mid75100 = 0
mid100125 = 0
mid125150 = 0
top150 = 0

for(j in 1:trials){
  price = current_price
  for(i in 1:remaining_days){
    move = sample(shifts,1) * (curr_price_weight * current_price + sim_price_weight * price) * u()
    price = price + move
  }
  cap = price*sharesOutstanding
  if(cap < 75) sub75 = sub75+1
  if(cap >= 75 & cap <= 100) mid75100 = mid75100 + 1
  if(cap > 100 & cap < 125) mid100125 = mid100125 + 1
  if(cap >= 125 & cap <= 150) mid125150 = mid125150 + 1
  if(cap > 150) top150 = top150 + 1
}

print(paste0("Below 75: ",round((sub75/trials)*100,1),"%, 75-100: ",round((mid75100/trials)*100,1),"%, 100-125: ",round((mid100125/trials)*100,1),"%, 125-150: ",round((mid125150/trials)*100,1),"%, 150+: ",round((top150/trials)*100,1),"%"))
