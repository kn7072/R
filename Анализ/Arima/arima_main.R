# http://www.quintuitive.com/2012/08/22/arma-models-for-trading/
library(quantmod)
library(fArma)
library(ggplot2)
library(lattice)
library(timeSeries)

#setwd(paste(getwd(),"/Анализ/Arima", sep=""))
source("arima_functions.R")

# Get S&P 500
from_data = "2000-01-01"
getSymbols("^GSPC", from=from_data)

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

best <- armaComputeForecasts(gspcRets, history=500, cores=2)
best.table <- data.frame(best)
write.table(best.table, file = "result.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = F, qmethod = c("escape", "double"),
            fileEncoding = "")

result_predict <- data.frame(data=index(gspcRets), gspcRets, best)
p <- ggplot(data=result_predict, aes(data, GSPC.Close)) + geom_line(colour="red") + geom_line(aes(data, Forecasts), colour="black")
p


getSymbols("^GSPC", from=from_data)
gspcRets <- Ad(GSPC) / lag(Ad(GSPC)) - 1
gspcArmaInd <- as.xts(read.zoo(file="result.csv", format="%Y-%m-%d", header=T, sep=","))
# Filter out only the common indexes
mm <- merge(gspcArmaInd[, 1], gspcRets, all=F)
gspcArmaRets <- mm[, 1] * mm[, 2]
gspcArmaGrowth <- log(cumprod(1 + gspcArmaRets))
gspcBHGrowth <- log(cumprod(1 + mm[, 2]))
gspcAllGrowth <- merge(gspcArmaGrowth, gspcBHGrowth, all=F)
xyplot(gspcAllGrowth, superpose=T, col=c("darkgreen", "darkblue"),
       lwd=2, key=list(x=.01, y=0.95, text=list(c("ARMA", "Buy-and-Hold")),
                        lines=list(lwd=2, col=c("darkgreen", "darkblue"))))









print("THE END")
###############################################################################################################################

getSymbols("^GSPC", from="2000-01-01")#1900-01-01

gspcRets <- Ad(GSPC) / lag(Ad(GSPC)) - 1
gspcRets[as.character(head(index(Ad(GSPC)), 1))] = 0

# The maximum draw down
head(drawdownsStats(as.timeSeries(gspcRets)), 10)

# The largest dropdawn is:
#         From     Trough         To      Depth Length ToTrough Recovery
# 1 2007-10-10 2009-03-09 2012-09-28 -0.5677539   1255      355       NA

# Load the ARMA indicator
gspcArmaInd <- as.xts(read.zoo(file="gspcInd3.csv", format="%Y-%m-%d", header=T, sep=","))

# Filter out only the common indexes
mm <- merge(gspcArmaInd[, 1], gspcRets, all=F)
gspcArmaRets <- mm[, 1] * mm[, 2]

# The maximum draw down
head(drawdownsStats(as.timeSeries(gspcArmaRets)), 10)
# The largest dropdawn is:
#          From     Trough         To      Depth Length ToTrough Recovery
# 1  1987-10-26 1992-10-09 1997-10-27 -0.5592633   2531     1255     1276

gspcArmaGrowth <- log(cumprod(1 + gspcArmaRets))

gspcBHGrowth <- log(cumprod(1 + mm[, 2]))

gspcAllGrowth <- merge(gspcArmaGrowth, gspcBHGrowth, all=F)

xyplot(gspcAllGrowth, superpose=T, col=c("darkgreen", "darkblue"),
       lwd=2, key=list(x=.01, y=0.95, text=list(c("ARMA", "Buy-and-Hold")),
       lines=list(lwd=2, col=c("darkgreen", "darkblue"))))





