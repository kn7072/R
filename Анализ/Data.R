require(xts)
require(quantmod)
require(PerformanceAnalytics)
require(TTR)
library(ggplot2)
library(grid)
library(gridExtra)
# rm(list=ls())
data <- '2005-01-01'  # '1990-01-01'
getSymbols('SPY', from = data, src = 'yahoo')

spy.table <- data.frame(Date=as.Date(as.character(index(SPY))), SPY)

sma_100 <- SMA(spy.table$'SPY.Close', 100)
sma_200 <- SMA(spy.table$'SPY.Close', 200)
aroon <- aroon(spy.table$'SPY.Close', 100)
volatility_ <- volatility(spy.table$'SPY.Close', 20)
rsi <- RSI(spy.table$'SPY.Close', 20)
roc <- ROC(spy.table$'SPY.Close', 20)

spy.table <- data.frame(spy.table, aroon, sma_100=sma_100, sma_200=sma_200, volatility=volatility_,
                        rsi=rsi, roc=roc)

spy.table <- na.omit(spy.table)
delta <- spy.table$SPY.Close - spy.table$SPY.Open
delta[delta > 0] <- 1
delta[delta < 0] <- -1
qplot(1:length(delta), delta)

spy.table$delta <- delta

library(class)
train = sample(1:nrow(spy.table), nrow(spy.table)/2)
test = (-train)
y.test = spy.table[test]
set.seed(1)
knn.pred <- knn()
