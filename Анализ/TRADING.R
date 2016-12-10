# http://www.r-bloggers.com/plotting-time-series-in-r-using-yahoo-finance-data/
library(ggplot2)
library(xts)
library(dygraphs)
library(TTR)

# Get IBM and Linkedin stock data from Yahoo Finance
ibm_url <- "http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
lnkd_url <- "http://real-chart.finance.yahoo.com/table.csv?s=LNKD&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"

yahoo.read <- function(url){
  dat <- read.table(url, header=TRUE, sep=",")
  df <- dat[, c(1,5)]
  df$Date <- as.Date(as.character(df$Date))
  return(df)}

ibm  <- yahoo.read(ibm_url)
lnkd <- yahoo.read(lnkd_url)


require("quantmod")
sp500 <- new.env()
getSymbols("SPY", env = sp500, src = "yahoo", from = as.Date("2000-01-04"), to = Sys.Date()) #2010-01-22 2000-01-04

set.seed(1)
macd <- MACD(ibm[, 2], 12, 26, 9, maType="EMA")
rsi <- RSI(ibm[, 2], n = 14)

qplot(rsi)


tlt <- new.env()
getSymbols("TLT", env = tlt, src = "yahoo", from = as.Date("2000-01-04"), to = Sys.Date())

brk_b <- new.env()
getSymbols("BRK-B", env = brk_b, src = "yahoo", from = as.Date("2010-01-22"), to = Sys.Date())
#tlt$TLT[1, 2]
# отбор сталбцов
my_vars <- names(tlt$TLT) %in% c("TLT.Close", "TLT.Volume")
my_data <- tlt$TLT[, my_vars]


# конвертация xts в list
datefield_tlt = index(tlt$TLT)
table_tlt <- data.frame(data_index_tlt=datefield_tlt, tlt$TLT,  row.names=NULL)

datefield_brk_b = index(brk_b$'BRK-B')
table_brk_b <- data.frame(datefield_brk_b, brk_b$'BRK-B',  row.names=NULL)

datefield_spy = index(sp500$SPY)
table_spy <- data.frame(data_index_sp=datefield_spy, sp500$SPY,  row.names=NULL)
table_spy_2 <- table_spy[table_spy$data_index_sp %in% table_tlt$data_index_tlt, ]

all_table <- data.frame(data=table_spy_2$data_index_sp, close_sp=table_spy_2$SPY.Close, diff=abs(table_spy_2$SPY.Close - table_spy_2$SPY.Open)*10,
                        value_sp=table_spy_2$SPY.Volume,    close_tlt=table_tlt$TLT.Close, value_tlt=table_tlt$TLT.Volume)

plot(all_table$close_sp, pch=20, lty=2, col="blue")#, ylim = c(25, 180)
lines(all_table$close_tlt, pch=20, lty=1, col="red")
summary(all_table)

#######################################################################
boxplot(log(all_table$close_sp), log(all_table$close_tlt))
hist(all_table$close_sp)


all_table_2 <- data.frame(data=table_spy_2$data_index_sp, close_sp=scale(table_spy_2$SPY.Close)*10+50,
                        value_sp=table_spy_2$SPY.Volume,    close_tlt=scale(table_tlt$TLT.Close)*10+50, value_tlt=table_tlt$TLT.Volume)
plot(all_table_2$close_sp, pch=20, lty=2, col="blue")#, ylim = c(25, 180)
lines(all_table_2$close_tlt, pch=20, lty=1, col="red")

all_scala <- data.frame(data=table_spy_2$data_index_sp, close_sp=table_spy_2$SPY.Close, close_tlt=table_tlt$TLT.Close)
tmp_all_scala <- scale(all_scala[, 2:3])
plot(tmp_all_scala[,2], lty=3, pch=20, lwd=1)
lines(tmp_all_scala[,1], col="red")

qqnorm(all_table$close_sp)
# qqnorm(log(all_table$close_sp +1))
qqline(all_table$close_sp)
shapiro.test(all_table$close_sp)

ks.test(all_table$close_sp, "pnorm")


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
# Проведем тест на равенство дисперсий  F-критерий  Фишера для нормально распределенных
table_spy_f <- data.frame(f=rep(c("spy"), length(sp500$SPY$SPY.Close)), x=as.vector(sp500$SPY$SPY.Close), fix.empty.names = F)
table_tlt_f <- data.frame(f=rep(c("tlt"), length(tlt$TLT$TLT.Close)), x=as.vector(tlt$TLT$TLT.Close), fix.empty.names = F)
all_table_rbind <- rbind(table_tlt_f, table_spy_f)
var.test(x ~ f, all_table_rbind)

###############################################################################################################################
# Критерий  Флигнера-Килина
library(car)
# Тест Левене
leveneTest(x ~ f, data = all_table_rbind)

leveneTest(x ~ f, data = all_table_rbind, center = mean)

# Аналогично  тесты  Бартлетта  и  Флигнера-Килина  выполняются  при  помощи
# функций bartlett.test() и fligner.test() соответственно:
  
bartlett.test(x ~ f, data = all_table_rbind)
fligner.test(x ~ f, data = all_table_rbind)

###############################################################################################################################
# Оценка корреляции двух случайных величин
# пирсон для нормально распределенных
cl_sp <- all_table$close_sp
cl_tlt <- all_table$close_tlt
cor.test(cl_sp, cl_tlt)
cor.test(log(cl_sp), log(cl_tlt))

cor.test(cl_sp, cl_tlt, method = "spearman")

cor.test(cl_sp, cl_tlt, method = "kendall")

# ?cor.test






# фильтрация по сторокам
#table_2 <- table[which(table$TLT.Open > 83 & table$TLT.Volume > 7000), ]  # 132

# leadership$date <- as.Date(leadership$date, "%m/%d/%y")
# startdate <- as.Date("2009-01-01")
# enddate <- as.Date("2009-10-31")
# newdata <- leadership[which(leadership$date >= startdate & leadership$date <= enddate),]
# 134
# newdata <- subset(leadership, age >= 35 | age < 24, select=c(q1, q2, q3, q4))
# newdata <- subset(leadership, gender == "M" & age > 25, select=gender:q4)

#par(mfrow=c(2,1))
newdata_tlt <- transform(table_tlt, TLT.Open = scale(TLT.Open)*10+50)
newdata_brk_b <- transform(table_brk_b, BRK.B.Open = scale(BRK.B.Open)*10+50)
newdata_spy <- transform(table_spy, SPY.Open = scale(SPY.Open)*10+50)
#axis(2, at=seq(from = 30, to = 80, by = 2), las=2)
plot(newdata_tlt$datefield_tlt, newdata_tlt$TLT.Open, pch=20, lty=2, col="blue", ylim = c(25, 180))

#plot(newdata_brk_b$datefield_brk_b, newdata_brk_b$BRK.B.Open, pch=20, lty=2, col="blue", ylim = c(25, 80))

lines(newdata_spy$datefield_spy, newdata_spy$SPY.Open, pch=20, lty=1, col="red")
#lines(newdata_brk_b$datefield_brk_b, newdata_brk_b$BRK.B.Open, pch=20, lty=1, col="red")







require(PerformanceAnalytics)
require(quantmod)
getSymbols("XLP", from="1990-01-01")
getSymbols("GLD", from="1990-01-01")
getSymbols("TLT", from="1990-01-01")
prices <- cbind(Ad(XLP), Ad(GLD), Ad(TLT))
prices <- prices[!is.na(prices[, 2]),]
rets <- Return.calculate(prices)
warthogRets <- Return.portfolio(rets, weights=c(.5, .15, .35), rebalance_on = "years")
getSymbols("SPY", from="1990-01-01")
SPYrets <- Return.calculate(Ad(SPY))
comparison <- merge(warthogRets, SPYrets, join='inner')
charts.PerformanceSummary(comparison)





require(IKTrading)
require(quantstrat)
require(PerformanceAnalytics)

initDate="1990-01-01"
from="2003-01-01"
to=as.character(Sys.Date())
options(width=70)

source("demoData.R")

#trade sizing and initial equity settings
tradeSize <- 100000
initEq <- tradeSize*length(symbols)

strategy.st <- portfolio.st <- account.st <- "VCI_test"
rm.strat(portfolio.st)
rm.strat(strategy.st)
initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)

#parameters
pctATR=.02
period=10

nRange=2
nLookback=10
pctRank=FALSE

buyThresh=-2
sellThresh=2

nSMA=200

#indicators
add.indicator(strategy.st, name="lagATR", 
              arguments=list(HLC=quote(HLC(mktdata)), n=period), 
              label="atrX")

add.indicator(strategy.st, name="VCI",
              arguments=list(OHLC=quote(OHLC(mktdata)), nLookback=nLookback,
                             nRange=nRange, pctRank=pctRank),
              label="vci")

add.indicator(strategy.st, name="SMA",
              arguments=list(x=quote(Cl(mktdata)), n=nSMA),
              label="sma")

#signals
add.signal(strategy.st, name="sigComparison",
           arguments=list(columns=c("Close", "SMA.sma"), relationship="gt"),
           label="filter")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="VC.vci", threshold=buyThresh, 
                          relationship="lt", cross=FALSE),
           label="VCIltThresh")

add.signal(strategy.st, name="sigAND",
           arguments=list(columns=c("filter", "VCIltThresh"), cross=TRUE),
           label="longEntry")

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="VC.vci", threshold=sellThresh,
                          relationship="gt", cross=TRUE),
           label="longExit")

add.signal(strategy.st, name="sigCrossover",
           arguments=list(columns=c("Close", "SMA.sma"), relationship="lt"),
           label="filterExit")

#rules
add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longEntry", sigval=TRUE, ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open", osFUN=osDollarATR,
                        tradeSize=tradeSize, pctATR=pctATR, atrMod="X"), 
         type="enter", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="filterExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

#apply strategy
t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st,portfolios=portfolio.st)
t2 <- Sys.time()
print(t2-t1)

#set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)
