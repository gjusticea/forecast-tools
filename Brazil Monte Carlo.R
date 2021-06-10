# Data from OWID at https://github.com/owid/covid-19-data/tree/master/public/data 

library(data.table)
dat = fread("~/GJP/OWID Brazil Raw.csv")

# I'm using Our World in Data, WHO data lags OWID data by two days
endDate = as.Date("2021-07-29")
dat$date = as.Date(dat$date,format="%m/%d/%Y")
dat$change = dat$new_cases_smoothed - c(0,unlist(dat[1:(nrow(dat)-1),"new_cases_smoothed"]))
dat$changePerc = (dat$new_cases_smoothed - c(0,unlist(dat[1:(nrow(dat)-1),"new_cases_smoothed"])))/c(0,unlist(dat[1:(nrow(dat)-1),"new_cases_smoothed"]))
dat = dat[c(2:nrow(dat)),]

# This lets me decide what period of time I want to sample from, I'm looking at consistently declining periods
dat$Category = NA
dat[which(dat$date >= as.Date("2020-08-09") & dat$date <= as.Date("2020-11-06")),"Category"] = "P1"
dat[which(dat$date >= as.Date("2021-03-24") & dat$date <= as.Date("2020-04-28")),"Category"] = "P2"


# I'm forecasting using 7-day average new cases, and summing the average under-counts the true total by 150k-200k
# The under-count varies by day of the week since new cases have a weekly pattern, usually hitting a trough Sunday-Monday
# July 29 is a Thursday, and the under-count on Thursdays is historically 3.22x that day's new smoothed cases number
diffRatio = 3.22

# Run the Monte Carlo
# This uses raw changes, 
getProbs = function(periods=c("P1","P2")){
  days = endDate - max(dat$date)
  shiftPool = as.numeric(unlist(dat[which(dat$Category %in% periods),"change"]))
  
  trials = 20000
  above19 = 0
  
  for(i in 1:trials){
    # Two variables because I need to net the final change for the running average adjustment
    runningSum = sum(dat$new_cases_smoothed,na.rm=TRUE)
    level = as.numeric(dat[nrow(dat),"new_cases_smoothed"])
    for(j in 1:days){
      dayChange = sample(shiftPool,1)
      level = level + dayChange
      if(j == days) lastLevel = level
      runningSum = runningSum + level
    }
    adjSum = runningSum + (diffRatio*lastLevel)
    if(adjSum > 19000000) above19 = above19 + 1
  }
  print(paste0("Above 19M: ",round((above19/trials)*100),"%"))
}

# Monte Carlo using probabilities instead of raw changes
getProbsPercent = function(periods=c("P1","P2")){
  days = endDate - max(dat$date)
  shiftPool = as.numeric(unlist(dat[which(dat$Category %in% periods),"changePerc"]))
  
  trials = 20000
  above19 = 0
  
  for(i in 1:trials){
    runningSum = sum(dat$new_cases_smoothed,na.rm=TRUE)
    level = as.numeric(dat[nrow(dat),"new_cases_smoothed"])
    for(j in 1:days){
      dayChange = sample(shiftPool,1)
      level = level * (1+dayChange)
      if(j == days) lastLevel = level
      runningSum = runningSum + level
    }
    adjSum = runningSum + (diffRatio*lastLevel)
    if(adjSum > 19000000) above19 = above19 + 1
  }
  print(paste0("Above 19M: ",round((above19/trials)*100),"%"))
}

getProbs()
getProbsPercent()

