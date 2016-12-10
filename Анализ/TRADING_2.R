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

x <- c("a", "b", "c")
y <- c("q", "w", "e")
f_1 <- factor(sample(x, length(rsi), replace = T))
f_2 <- factor(sample(y, length(rsi), replace = T))
data.rsi <- na.omit(data.frame(rsi=rsi, macd=macd[, 1], f_1=f_1, f_2=f_2))

qplot(f_1, rsi, data=data.rsi, geom="jitter", colour=f_2)
qplot(macd, rsi, data=data.rsi, geom=c("point", "smooth"), facets=f_1 ~ f_2)

qplot(f_1, rsi, data=data.rsi, geom="boxplot")


