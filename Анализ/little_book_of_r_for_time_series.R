library("TTR")
library("forecast")
library(timeSeries)
library(tseries) 
# http://zoonek2.free.fr/UNIX/48_R/all.html
# http://zoonek2.free.fr/UNIX/48_R/15.html#8
# http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings

kingstimeseries <- ts(kings)
kingstimeseries

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
plot.ts(birthstimeseries)


souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries
plot.ts(souvenirtimeseries)
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

# Decomposing Non-Seasonal Data
kingstimeseriesSMA3 <- SMA(kingstimeseries, n=3)
plot.ts(kingstimeseriesSMA3)

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

# Decomposing Seasonal Data
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal # get the estimated values of the seasonal component
plot(birthstimeseriescomponents)


birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)


# Forecasts using Exponential Smoothing
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
rainseriesforecasts$fitted

plot(rainseriesforecasts)


rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
rainseriesforecasts2
plot.forecast(rainseriesforecasts2)
acf(na.omit(rainseriesforecasts2$residuals), lag.max=20)
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
plot.ts(rainseriesforecasts2$residuals)

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(na.omit(rainseriesforecasts2$residuals))

#################################################################################################
# Holt’s Exponential Smoothing
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts
skirtsseriesforecasts$SSE
plot(skirtsseriesforecasts)


# ARIMA Models
skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1)

skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)
adf.test(skirtsseriesdiff2, alternative="stationary")
kpss.test(skirtsseriesdiff2) # нулевая гипотиза - ряд стационарный

kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1)
adf.test(kingtimeseriesdiff1)  # стационарен  p-value < 0.05 => stationary
kpss.test(kingtimeseriesdiff1)  # стационарен

acf(kingtimeseriesdiff1, lag.max=20)  # plot a correlogram
pacf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get the partial autocorrelation values
pacf(kingtimeseriesdiff1, lag.max=20)

kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
kingstimeseriesforecasts <- forecast.Arima(kingstimeseriesarima, h=5)
kingstimeseriesforecasts
plot.forecast(kingstimeseriesforecasts)

acf(kingstimeseriesforecasts$residuals, lag.max=20)
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")

plot.ts(kingstimeseriesforecasts$residuals)            # make time plot of forecast errors
plotForecastErrors(kingstimeseriesforecasts$residuals) # make a histogram

#####################################################################################

library(seasonal)
library(Mcomp)
seas(M3[[434]]$x, regression.aictest = NULL)
# https://www.r-bloggers.com/x13-seats-arima-as-an-automated-forecasting-tool/

