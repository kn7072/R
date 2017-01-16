# http://www.quintuitive.com/2012/08/22/arma-models-for-trading/
library(quantmod)
library(fGarch)
library(car)
library(ggplot2)

setwd(paste(getwd(),"/Анализ/Arima", sep=""))
source("garchAuto.R")

data_from <- "2010-01-01"  # "1900-01-01"
getSymbols("SPY", from=data_from)
spyRets <- diff(log(Ad(SPY)))
table.spyRets <- data.frame(spyRets[, ])
#############################################################################
shapiro.test(table.spyRets[, 1])
qqPlot(spyRets, dist = "norm", col = palette()[1], pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭР и НР")

qplot(table.spyRets[, 1])
plot(spyRets)
#############################################################################
best_model <- garchAuto(as.ts(tail(spyRets, 500)))
predict(best_model, n.ahead=1, doplot=F)










spyGarch <- garchFit(~arma(0, 2) + garch(1, 1), data=as.ts(tail(spyRets, 500)))
predict(spyGarch, n.ahead=1, doplot=F)
# the actual forecasts are predict(spyGarch, n.ahead=1, doplot=F)[,1]