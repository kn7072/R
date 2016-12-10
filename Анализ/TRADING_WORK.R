require(xts)
require(quantmod)
require(PerformanceAnalytics)
require(TTR)
library(ggplot2)
library(grid)
library(gridExtra)

getSymbols('SPY', from = '1990-01-01', src = 'yahoo')
adjustedPrices <- Ad(SPY)
monthlyAdj <- to.monthly(adjustedPrices, OHLC=TRUE)

spySMA <- SMA(Cl(monthlyAdj), 10)
spyROC <- ROC(Cl(monthlyAdj), 10)
spyRets <- Return.calculate(Cl(monthlyAdj))

smaRatio <- Cl(monthlyAdj)/spySMA - 1
smaSig <- smaRatio > 0
rocSig <- spyROC > 0

smaRets <- lag(smaSig) * spyRets
rocRets <- lag(rocSig) * spyRets

strats <- na.omit(cbind(smaRets, rocRets, spyRets))
colnames(strats) <- c("SMA10", "MOM10", "BuyHold")
charts.PerformanceSummary(strats, main = "strategies")
rbind(table.AnnualizedReturns(strats), maxDrawdown(strats), CalmarRatio(strats))

###########################################################################################

spy.table <- data.frame(Date=as.Date(as.character(index(SPY))), SPY)
# SMA 100
spySMA_temp <- SMA(spy.table$'SPY.Close', 100)
spySMA <- ifelse(!is.na(spySMA_temp), spySMA_temp, spy.table$'SPY.Close'[1])
# SMA 200
spySMA200_temp <- SMA(spy.table$'SPY.Close', 200)
spySMA200 <- ifelse(!is.na(spySMA200_temp), spySMA200_temp, spy.table$'SPY.Close'[1])


# par(mfrow=c(3, 1))
x_1 <- na.omit(data.frame(spy.table, sma=spySMA, sma_200=spySMA200))
p <- ggplot(data=x_1) 
p <- p + geom_line(aes(x=Date, y=sma_200), colour="black")  
p <- p + geom_line(aes(x=Date, y=SPY.Close), colour="blue")  
p <- p + geom_line(aes(x=Date, y=sma), colour="red") 
#p #+ scale_y_continuous()
#p + theme_gray()
#p + theme_light()


q <- ggplot(data=x_1) 
q <- q + geom_line(aes(x=Date, y=sma_200), colour="black") 
#q

parenttvp <- viewport(layout = grid.layout(2, 2))
pushViewport(parenttvp)
print(p, vp=viewport(layout.pos.row = 1, layout.pos.col = c(1, 2)))
print(q, vp=viewport(layout.pos.row = 2, layout.pos.col = c(1, 2)))




p1 = qplot(1:10, rnorm(10))
p2 = qplot(1:10, rnorm(10))
grid.arrange(p1, p2, ncol = 2, main = "Main title")
