require(xts)
require(quantmod)
require(PerformanceAnalytics)
require(TTR)
library(ggplot2)
library(grid)
library(gridExtra)
library(timeSeries)
library(tseries) 
library(car)
library(forecast)

# rm(list=ls())
data <- '2015-01-01'  # '1990-01-01'
getSymbols('SPY', from = data, src = 'yahoo') # SPY

diff_log <- na.omit(ts(diff(log(Ad(SPY)))))

#spyRets_x <- diff_log - mean(diff_log)
spyRets_x <- diff_log
decompose(spyRets_x)


summary(powerTransform(SPY$SPY.Close))
shapiro.test(as.vector(spyRets_x))  # SPY$SPY.Close

# тесты на ГЕТЕРОСКЕДАНТИЧНОСТЬ - БРОЙША-ПАГАНА, ГОЛДФЕЛДА-КВАДРА



Box.test(spyRets_x)  # 378  тест на автокорреляцию if p-value < 0.05 => stationary
adf.test(spyRets_x, alternative="stationary") # нулевая гипотеза - ряд НЕ стацаонарный For the adf.test, if p-value < 0.05 => stationary
kpss.test(spyRets_x) # нулевая гипотиза - ряд стационарный For the kpss.test, if p-value > 0.05 => stationary (note change of inequality)

ma5 = filter(spyRets_x, sides=2, rep(1,5)/5)
ma53 = filter(spyRets_x, sides=2, rep(1,53)/53)
plot(spyRets_x, type="p", ylab="mortality", pch=1, cex=2, lty=1)
lines(ma5); lines(ma53)
acf(spyRets_x)
acf(spyRets_x, plot=F)
acf(as.vector(spyRets_x),lag.max=36)
pacf(spyRets_x)
pacf(spyRets_x, plot=F)


require(forecast)
ARIMAfit <- auto.arima(spyRets_x, approximation=FALSE, trace=T)
summary(ARIMAfit)


fit <- auto.arima(spyRets_x, seasonal=FALSE, trace=TRUE, test="kpss",  ic="bic")
ar_fit <- arima(spyRets_x, order = c(1, 0, 1))
plot(ar_fit$residuals)
#residuals(ar_fit)
fcast2 <- forecast(ar_fit, h=12)
plot(fcast2, include=50)


rainseriesforecasts <- HoltWinters(spyRets_x, beta=FALSE, gamma=FALSE)
plot(rainseriesforecasts, type="p", ylab="mortality", pch=1, cex=2, lty=1)

plot(spyRets_x, type="p", ylab="mortality")
lines(ksmooth(time(spyRets_x), spyRets_x, "normal", bandwidth=5/52))
lines(ksmooth(time(spyRets_x), spyRets_x, "normal", bandwidth=2))




spyRets <- matrix(diff(log(Ad(SPY))))

spy.table <- data.frame(Date=as.Date(as.character(index(SPY))), SPY)

sma_100 <- SMA(spy.table$'SPY.Close', 100)
sma_200 <- SMA(spy.table$'SPY.Close', 200)
aroon <- aroon(spy.table$'SPY.Close', 100)
volatility_ <- volatility(spy.table$'SPY.Close', 20)
rsi <- RSI(spy.table$'SPY.Close', 20)
roc <- ROC(spy.table$'SPY.Close', 20)

spy.table <- data.frame(spy.table, aroon, sma_100=sma_100, sma_200=sma_200, volatility=volatility_, rsi=rsi, roc=roc, diff=spyRets)
spy.table <- na.omit(spy.table)


y <- cbind(spy.table$diff, spy.table$rsi, spy.table$roc)
cor(y)
qplot(1:length(spy.table$diff), spy.table$diff)
qplot(diff(log(Ad(SPY))))
boxplot(spy.table$diff)


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



library(seasonal)







