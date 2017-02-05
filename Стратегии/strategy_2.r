require(quantmod)
require(TTR)
require(PerformanceAnalytics)

getSymbols('SPY', from = '1990-01-01')
monthlySPY <- Ad(SPY)[endpoints(SPY, on = 'months')]
monthlySPYrets <- Return.calculate(monthlySPY)
#dividing by 10 since that's the moving average period for comparison
signalTSMOM <- (monthlySPY - lag(monthlySPY, 10))/10 
signalDiffMA <- diff(SMA(monthlySPY, 10))

# rounding just 
sum(round(signalTSMOM, 3)==round(signalDiffMA, 3), na.rm=TRUE)

toyStrat <- monthlySPYrets * lag(signalTSMOM > 0)

emaSignal <- diff(EMA(monthlySPY, 10))
emaStrat <- monthlySPYrets * lag(emaSignal > 0)

comparison <- cbind(toyStrat, emaStrat)
colnames(comparison) <- c("DiffSMA10", "DiffEMA10")
charts.PerformanceSummary(comparison)
table.AnnualizedReturns(comparison)