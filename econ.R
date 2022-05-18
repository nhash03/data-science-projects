library(quantmod)
library(xts)
library(tidyverse)
library(TTR)
getSymbols("AAPL")
AAPL <- last(AAPL, '5 years')
Open <- Op(AAPL)   #Open Price
High <- Hi(AAPL)    # High price
Low <- Lo(AAPL)  # Low price
Close<- Cl(AAPL)   #Close Price
Volume <- Vo(AAPL)   #Volume
AdjClose <- Ad(AAPL) # Adjusted close
TotTradeVolWeekly <- apply.weekly(Vo(AAPL), sum)
AveTradeVolWeekly <- apply.weekly(Vo(AAPL), mean)
head(TradeVolWeekly)
head(AveTradeVolWeekly)
chartSeries(AAPL,
            type = "candlestick",
            subset = '2021-05-10::2021-07-09',
            theme = chartTheme('black'))
sma <- SMA(Cl(AAPL))
ema <- EMA(Cl(AAPL))
addSMA(n=5, on= 1, col = 'yellow')
addSMA(n=10, on= 1, col = 'purple')

price <- Cl(AAPL)
delta <- 0.003
r <- price/Lag(price) -1
signal <- c(0)

for (i in 2: length(price)) {
  if (r[i] > delta) {
    signal[i] <- 1
  }
  else if (r[i] < -delta){
    signal[i] <- -1
  }
  else {
    signal[i] <- 0
  }
}
signal <- reclass(signal, price)
addTA(signal, type= 'S', col= "red")