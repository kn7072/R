# http://www.quintuitive.com/2012/08/22/arma-models-for-trading/
library(quantmod)
library(fArma)
library(ggplot2)

#setwd(paste(getwd(),"/Анализ/Arima", sep=""))
source("arima_functions.R")

# Get S&P 500
getSymbols("^GSPC", from="2014-01-01")

# Compute the daily returns
gspcRets <- diff(log(Cl(GSPC)))

# Use only the last two years of returns
data_train <- tail(gspcRets, 500)
spyRets <- as.ts(data_train)
table_train <- data.frame(date=index(data_train), data.frame(data_train))

# Fit the model
spyArma <- armaFit(formula=~arma(2, 2), data=gspcRets)
spyArma@fit$aic

# ПРОГНОЗИРОВАНИЕ
as.numeric(predict(spyArma, n.ahead=1, doplot=F)$pred)

# поиск лучшей модели
result_models <- armaSearch(spyRets, minOrder=c(0, 0), maxOrder=c(5, 5))

best <- armaComputeForecasts(gspcRets, history=50, cores=1)
result_predict <- data.frame(data=index(gspcRets), gspcRets, best)

p <- ggplot(data=result_predict, aes(data, GSPC.Close)) + geom_line(colour="red") + geom_line(aes(data, Forecasts), colour="black")
p

print("THE END")
