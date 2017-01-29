# https://inovancetech.com/hmm-tutorial-1.html
library(quantmod)
library(depmixS4)
library(xts)
library(forecast)
setwd("D:/R_project/Анализ/HMM")
EURUSD1d <- read.csv("EURUSD1d.csv")

Date<-as.character(EURUSD1d[, 1])
DateTS<- as.POSIXlt(Date, format = "%Y.%m.%d %H:%M:%S")  # create date and time objects
TSData<-data.frame(EURUSD1d[, 2:5],row.names=DateTS)
TSData<-as.xts(TSData)  # build our time series data set
tsdisplay(TSData)

ATRindicator<-ATR(TSData[, 2:4], n=14)  # calculate the indicator
ATR<-ATRindicator[, 2]  # grab just the ATR

LogReturns <- log(EURUSD1d$Close) - log(EURUSD1d$Open)  # calculate the logarithmic returns
ModelData<-data.frame(LogReturns,ATR)  # create the data frame for our HMM model
ModelData<-ModelData[-c(1:14),]  # remove the data where the indicators are being calculated
colnames(ModelData)<-c("LogReturns","ATR")  # name our columns


set.seed(1)
HMM<-depmix(list(LogReturns~1, ATR~1), data=ModelData, nstates=3, family=list(gaussian(), gaussian()))  # We’re setting the LogReturns and ATR as our response variables, using the data frame we just built, want to set 3 different regimes, and setting the response distributions to be gaussian.
HMMfit<-fit(HMM, verbose = FALSE)  # fit our model to the data set
print(HMMfit)  # we can compare the log Likelihood as well as the AIC and BIC values to help choose our model

summary(HMMfit)
HMMpost<-posterior(HMMfit)  # find the posterior odds for each state over our data set
head(HMMpost)  # we can see that we now have the probability for each state for everyday as well as the highest probability class.

par(mfrow=c(3, 1))
plot(HMMpost$S1, col="darkviolet", type="l", lwd=2)
plot(HMMpost$S2, col="darkorange1", type="l", lwd=2)
plot(HMMpost$S3, col="darkolivegreen4", type="l", lwd=2)


par(mfrow=c(3, 1))
plot(ModelData$LogReturns, col="darkviolet", type="l", lwd=2)
plot(ModelData$ATR, col="darkorange1", type="l", lwd=2)
plot(HMMpost$state, col="darkolivegreen4", type="l", lwd=2)
# What we can see is that regime 3 tends to be times of high volatility and large magnitude moves, 
# regime 2 is characterized by medium volatility, 
# and regime 1 consists of low volatility.
