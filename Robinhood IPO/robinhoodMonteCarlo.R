getIPO = function(){
  price = rnorm(1,35,3)
  
  mkupYN = runif(1) > 0.8
  if(mkupYN == TRUE){
    price = price*rnorm(1,1.15,.035)
  }
  price = price*rnorm(1,1.30,.18)
  
  return(price)
}

prices = c()
for(i in 1:9999){
  prices[[i]] = getIPO()
}
prices = as.numeric(prices)
hist(prices)
