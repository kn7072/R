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

parenttvp <- viewport(layout = grid.layout(2, 2))
pushViewport(parenttvp)



spy.table <- data.frame(Date=as.Date(as.character(index(SPY))), SPY)
# SMA 100
spySMA_temp <- SMA(spy.table$'SPY.Close', 100)
spySMA <- ifelse(!is.na(spySMA_temp), spySMA_temp, spy.table$'SPY.Close'[1])
# SMA 200
spySMA200_temp <- SMA(spy.table$'SPY.Close', 200)
spySMA200 <- ifelse(!is.na(spySMA200_temp), spySMA200_temp, spy.table$'SPY.Close'[1])

#############################################################################################
# aroon
aroon <- aroon(spy.table$'SPY.Close', 100)
spy.table <- data.frame(spy.table, aroon)
spy.table <- na.omit(spy.table)

p <- ggplot(data=spy.table, aes(Date, SPY.Close)) + geom_line(colour="red")
q <- ggplot(data=spy.table) 
q <- q + geom_line(aes(Date, aroonUp), colour="red")
q <- q + geom_line(aes(Date, aroonDn), colour="blue")
q <- q + geom_line(aes(Date, oscillator), colour="black")

print(p, vp=viewport(layout.pos.row = 1, layout.pos.col = c(1, 2)))
print(q, vp=viewport(layout.pos.row = 2, layout.pos.col = c(1, 2)))

#############################################################################################
# Donchian Channel
parenttvp <- viewport(layout = grid.layout(1, 1))
pushViewport(parenttvp)
donchianchannel <- DonchianChannel(spy.table$'SPY.Close', 100)
spy.table$x <- donchianchannel
spy.table <- na.omit(spy.table)

q <- ggplot(data=spy.table, aes(Date, SPY.Close)) + geom_line(colour="blueviolet")
q <- q + geom_line(aes(Date, x[, 1]), colour="red")
q <- q + geom_line(aes(Date, x[, 2]), colour="blue")
q <- q + geom_line(aes(Date, x[, 3]), colour="black")

print(q, vp=viewport(layout.pos.row = 1, layout.pos.col = c(1, 1)))

#############################################################################################
# Volatility
parenttvp <- viewport(layout = grid.layout(2, 1))
pushViewport(parenttvp)
volatility_ <- volatility(spy.table$'SPY.Close', 20)
spy.table$x <- volatility_
spy.table <- na.omit(spy.table)

q <- ggplot(data=spy.table, aes(Date, SPY.Close)) + geom_line(colour="blueviolet")
p <- ggplot(data=spy.table) + geom_line(aes(Date, x), colour="darkgreen")

print(p, vp=viewport(layout.pos.row = 2, layout.pos.col = c(1, 1)))
print(q, vp=viewport(layout.pos.row = 1, layout.pos.col = c(1, 1)))
#############################################################################################
# RSI
parenttvp <- viewport(layout = grid.layout(2, 1))
pushViewport(parenttvp)
rsi <- RSI(spy.table$'SPY.Close', 20)
spy.table$x <- rsi
spy.table <- na.omit(spy.table)

q <- ggplot(data=spy.table, aes(Date, SPY.Close)) + geom_line(colour="blueviolet")
p <- ggplot(data=spy.table) + geom_line(aes(Date, x), colour="darkgreen")

print(p, vp=viewport(layout.pos.row = 2, layout.pos.col = c(1, 1)))
print(q, vp=viewport(layout.pos.row = 1, layout.pos.col = c(1, 1)))

#############################################################################################
# Rate of Change / Momentum
parenttvp <- viewport(layout = grid.layout(2, 1))
pushViewport(parenttvp)
roc <- ROC(spy.table$'SPY.Close', 20)
spy.table$x <- roc
spy.table <- na.omit(spy.table)

q <- ggplot(data=spy.table, aes(Date, SPY.Close)) + geom_line(colour="blueviolet")
p <- ggplot(data=spy.table) + geom_line(aes(Date, x), colour="darkgreen")

print(p, vp=viewport(layout.pos.row = 2, layout.pos.col = c(1, 1)))
print(q, vp=viewport(layout.pos.row = 1, layout.pos.col = c(1, 1)))

#############################################################################################
# chaikinVolatility
parenttvp <- viewport(layout = grid.layout(2, 1))
pushViewport(parenttvp)
chaikin_vol <- chaikinVolatility(spy.table[, c('SPY.High', 'SPY.Low')])
spy.table$x <- chaikin_vol
spy.table <- na.omit(spy.table)

q <- ggplot(data=spy.table, aes(Date, SPY.Close)) + geom_line(colour="blueviolet")
p <- ggplot(data=spy.table) + geom_line(aes(Date, x), colour="darkgreen")

print(p, vp=viewport(layout.pos.row = 2, layout.pos.col = c(1, 1)))
print(q, vp=viewport(layout.pos.row = 1, layout.pos.col = c(1, 1)))

#############################################################################################
# True Range / Average True Range
parenttvp <- viewport(layout = grid.layout(2, 1))
pushViewport(parenttvp)
atr <-  ATR(SPY[, c('SPY.High', 'SPY.Low', 'SPY.Close')], n=14)


ddd <- !is.na(atr$atr)
spy.table$x <- ifelse(ddd, as.numeric(atr$atr), 0)
spy.table <- na.omit(spy.table)

q <- ggplot(data=spy.table, aes(Date, SPY.Close)) + geom_line(colour="blueviolet")
p <- ggplot(data=spy.table) + geom_line(aes(Date, x), colour="darkgreen")

print(p, vp=viewport(layout.pos.row = 2, layout.pos.col = c(1, 1)))
print(q, vp=viewport(layout.pos.row = 1, layout.pos.col = c(1, 1)))

#############################################################################################

qplot(spy.table$Date, spy.table$x[, 3]) + geom_line(colour="red")
plot(spy.table$x[, 3])
