require(e1071)
require(quantmod)
require(parallel)
library(lattice)
library(timeSeries)
# http://www.quintuitive.com/2012/11/30/trading-with-support-vector-machines-svm/
setwd("D:/R_project/Анализ/svm/svm_1")
source("e1071.R")

tt = get( getSymbols( "^GSPC", from="1990-01-01" ) )

rets = na.trim( ROC( Cl( tt ), type="discrete" ) )
plot(rets)
# only the first two features so that we may see some results in reasonable time
data = svmFeatures( tt )[,c(1,2,3,4,5)]

rets = rets[index(data)]
data = data[index(rets)]

stopifnot( NROW( rets ) == NROW( data ) )

fore = svmComputeForecasts(
  data=data,
  history=500,
  response=rets,
  cores=1,
  trace=T,
  modelPeriod="days",
  startDate="2015-01-01",
  endDate="2017-01-01",
  featureSelection="all")

class(fore)
mm <- merge(fore[, 1], rets, all = F)
plot.zoo(mm)

gspcArmaRets <- mm[, 1] * mm[, 2]
gspcArmaGrowth = log(cumprod(1 + gspcArmaRets))
gspcBHGrowth = log(cumprod(1 + mm[,2] ) )
gspcAllGrowth = merge(gspcArmaGrowth, gspcBHGrowth, all=F)

xyplot(gspcAllGrowth, superpose=T, col=c("blueviolet", "brown2"), lwd=2,
        key=list(x=.01, y=0.95, text=list(c("SVM", "Buy-and-Hold")),
        lines=list(lwd=2, col=c("blueviolet", "brown2"))))



