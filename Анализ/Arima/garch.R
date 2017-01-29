# Quantitative Trading with R   Harry Georgakopoulos

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
data <- '2006-01-01'  # '1990-01-01'
getSymbols('SPY', from = data, src = 'yahoo')

diff_log <- na.omit(ts(diff(log(Ad(SPY)))))

prices <- SPY$SPY.Adjusted
mean_prices <- round(mean(prices), 2)
sd_prices <- round(sd(prices), 2)

# Plot the histogram along with a legend
hist(prices, breaks = 100, prob=T, cex.main = 0.9)
abline(v = mean_prices, lwd = 2)
legend("topright", cex = 0.8, border = NULL, bty = "n", paste("mean=", mean_prices, "; sd=", sd_prices))

returns <- diff(log(prices))

plot_4_ranges <- function(data, start_date, end_date, title){
  # Set the plot window to be 2 rows and 2 columns
  par(mfrow = c(2, 2))
  for(i in 1:4) {
    # Create a string with the appropriate date range
    range <- paste(start_date[i], "::", end_date[i], sep = "")
    # Create the price vector and necessary statistics
    time_series <- data[range]
    mean_data <- round(mean(time_series, na.rm = TRUE), 3)
    sd_data <- round(sd(time_series, na.rm = TRUE), 3)
    # Plot the histogram along with a legend
    hist_title <- paste(title, range)
    hist(time_series, breaks = 100, prob=TRUE,
         xlab = "", main = hist_title, cex.main = 0.8)
    legend("topright", cex = 0.7, bty = 'n',
           paste("mean=", mean_data, "; sd=", sd_data))
  }
  # Reset the plot window
  par(mfrow = c(1, 1))
}
    
begin_dates <- c("2007-01-01", "2008-06-06", "2009-10-10", "2011-03-03")
end_dates <- c("2008-06-05", "2009-09-09", "2010-12-30", "2013-01-06")
# Create plots
plot_4_ranges(prices, begin_dates, end_dates, "SPY prices for:")

require(urca)
getSymbols("SPY")
spy <- SPY$SPY.Adjusted

test <- ur.kpss(as.numeric(spy))

spy_returns <- diff(log(spy))
# Test on the returns
test_returns <- ur.kpss(as.numeric(spy_returns))
test_returns@teststat
test_returns@cval

# 101
# Set plotting window
par(mfrow = c(1, 2))
# SPY data
qqnorm(as.numeric(returns),
       main = "SPY empirical returns qqplot()",
       cex.main = 0.8)
qqline(as.numeric(returns), lwd = 2)
grid()
# Normal random data
normal_data <- rnorm(nrow(returns), mean = mu, sd = sigma)
qqnorm(normal_data, main = "Normal returns", cex.main = 0.8)
qqline(normal_data, lwd = 2)
grid()


answer <- shapiro.test(as.numeric(returns))
answer[[2]]
#########################################################################
pepsi <- getSymbols('PEP', from = '2013-01-01',
                    to = '2014-01-01', adjust = T, auto.assign = FALSE)
coke <- getSymbols('COKE', from = '2013-01-01',
                   to = '2014-01-01', adjust = T, auto.assign = FALSE)
Sys.setenv(TZ = "UTC")
prices <- cbind(pepsi[, 6], coke[, 6])
price_changes <- apply(prices, 2, diff)
plot(price_changes[, 1], price_changes[, 2],
     xlab = "Coke price changes",
     ylab = "Pepsi price changes",
     main = "Pepsi vs. Coke",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
grid()

ans <- lm(price_changes[, 1] ~ price_changes[, 2])
beta <- ans$coefficients[2]
#########################################################################
# Get the data
SPY <- getSymbols('SPY', from = '2011-01-01',
                  to = '2012-12-31', adjust = T, auto.assign = FALSE)
AAPL <- getSymbols('AAPL', from = '2011-01-01',
                   to = '2012-12-31', adjust = T, auto.assign = FALSE)
# Compute price differences
x <- diff(as.numeric(SPY[, 4]))
y <- diff(as.numeric(AAPL[, 4]))
plot(x, y, main = "Scatter plot of returns. SPY vs. AAPL", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
abline(lm(y ~ x))
abline(lm(x ~ y), lty = 2)
grid()
# Total least squares regression
r <- prcomp( ~x+y)
slope <- r$rotation[2, 1] / r$rotation[1, 1]
intercept <- r$center[2] - slope * r$center[1]
# Show the first principal component on the plot
abline(a = intercept, b = slope, lty = 3)

#########################################################################

symbols <- c("XLB", #SPDR Materials sector
             "XLE", #SPDR Energy sector
             "XLF", #SPDR Financial sector
             "XLP", #SPDR Consumer staples sector
             "XLI", #SPDR Industrial sector
             "XLU", #SPDR Utilities sector
             "XLV", #SPDR Healthcare sector
             "XLK", #SPDR Tech sector
             "XLY", #SPDR Consumer discretionary sector
             "RWR", #SPDR Dow Jones REIT ETF
             "EWJ", #iShares Japan
             "EWG", #iShares Germany
             "EWU", #iShares UK
             "EWC", #iShares Canada
             "EWY", #iShares South Korea
             "EWA", #iShares Australia
             "EWH", #iShares Hong Kong
             "EWS", #iShares Singapore
             "IYZ", #iShares U.S. Telecom
             "EZU", #iShares MSCI EMU ETF
             "IYR", #iShares U.S. Real Estate
             "EWT", #iShares Taiwan
             "EWZ", #iShares Brazil
             "EFA", #iShares EAFE
             "IGE", #iShares North American Natural Resources
             "EPP", #iShares Pacific Ex Japan
             "LQD", #iShares Investment Grade Corporate Bonds
             "SHY", #iShares 1-3 year TBonds
             "IEF", #iShares 3-7 year TBonds
             "TLT" #iShares 20+ year Bonds
)
from = '2011-01-01'
to = '2012-12-31'
if(!"XLB" %in% ls()) {
  # If data is not present, get it from yahoo
  suppressMessages(getSymbols(symbols, from = from,
                              to = to, src = "yahoo", adjust = TRUE))
}

stock(symbols, currency = "USD", multiplier = 1)
###############################################################################
library(fBasics)
setwd("D:/R_project/Analysis of Financial Time Series 3rd Edition/code")
da=read.table("d-ibm3dx7008.txt",header=T)
da
dim(da)

library(TSA)
data(oil.price)
plot(oil.price, ylab='Price per Barrel',type='l')
tsdisplay(oil.price)
























