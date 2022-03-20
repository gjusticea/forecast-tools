library(tidyverse)
library(quantmod)


strt = "2021-03-20"
shiftsizeThold = 0.15

#1/2 chance of sign flip
u = function() return(sample(c(-1,1),1))

#pull the most recent data on FB from Yahoo via quantmod package
getSymbols("FB",from=strt,warnings = FALSE,auto.assign = TRUE)
FB = FB[c(1:(nrow(FB)-1))]

#see what the most recent day with available data is (data for the current day isn't always available)
mostRecentDate = max(as.Date(rownames(as.data.frame(FB))))
FB = FB[which(!is.na(FB$`FB.Close`)),]

expiration = as.Date("2022-05-20")
current_price = as.numeric(FB[nrow(FB),"FB.Close"])

remaining_days = as.numeric(expiration - mostRecentDate)
trials = 10000
curr_price_weight = 0.2
sim_price_weight = 1-curr_price_weight

# Get a set of price shifts

df = as.data.frame(FB$`FB.Close`)
df$Previous = c(NA,df[c(1:(nrow(df)-1)),"FB.Close"])
df = df[which(!is.na(df$Previous)),]
df$dte = as.Date(rownames(df))
shifts = (df$`FB.Close`/df$Previous)-1
shifts = shifts[abs(shifts) < shiftsizeThold]

thold = 238

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


