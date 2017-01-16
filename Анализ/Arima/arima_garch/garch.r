### ARIMA/GARCH trading model
# http://robotwealth.com/fitting-time-series-models-to-the-forex-market-are-arimagarch-predictions-profitable/
library(quantmod)
library(timeSeries)
library(rugarch)
setwd("D:/R_project/Анализ/Arima/arima_garch")
# get data and initialize objects to hold forecasts
EURUSD <- read.csv('EURUSD.csv', header = T)
EURUSD[, 1] <- as.Date(as.character(EURUSD[, 1]), format="%d/%m/%Y") # change this to remove subset in final
returns <- diff(log(EURUSD$C)) ## ttr::ROC calcualtes log returns by default
window.length <- 1000
forecasts.length <- length(returns) - window.length
forecasts <- vector(mode="numeric", length=forecasts.length) 
directions <- vector(mode="numeric", length=forecasts.length) 
p.val <- vector(mode="numeric", length=forecasts.length) 

# loop through every trading day, estimate optimal model parameters from rolling window
# and predict next day's return
for (i in 0:forecasts.length) {
  roll.returns <- returns[(1+i):(window.length + i)] # create rolling window
  final.aic <- Inf
  final.order <- c(0,0,0)
  # estimate optimal ARIMA model order
  for (p in 0:5) for (q in 0:5) { # limit possible order to p,q <= 5
    if (p == 0 && q == 0) next # p and q can't both be zero
    arimaFit <- tryCatch( arima(roll.returns, order = c(p,0,q)), 
                          error = function( err ) FALSE,
                          warning = function( err ) FALSE )
    if (!is.logical( arimaFit)) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) { # retain order if AIC is reduced
        final.aic <- current.aic
        final.order <- c(p,0,q)
        final.arima <- arima(roll.returns, order = final.order)
      }
    }
    else next 
  }

  # specify and fit the GARCH model
  spec = ugarchspec(variance.model <- list(garchOrder=c(1,1)),
                    mean.model <- list(
                      armaOrder <- c(final.order[1], final.order[3]), include.mean = T),
                    distribution.model = "sged")
  fit = tryCatch(ugarchfit(spec, roll.returns, solver = 'hybrid'), error = function(e) e, warning = function(w) w)
  
  
  # make next day prediction from fitted mode.
  # model does not always converge - assign value of 0 to prediction and p.val in this case
  if (is(fit, "warning")) {
    forecasts[i+1] <- 0 
    print(0)
    p.val[i+1] <- 0
  }
  else {
    next.day.fore = ugarchforecast(fit, n.ahead = 1)
    x = next.day.fore@forecast$seriesFor
    directions[i+1] <- ifelse(x[1] > 0, 1, -1) # directional prediction only
    forecasts[i+1] <- x[1] # actual value of forecast
    print(forecasts[i])
    # analysis of residuals
    resid <- fit@fit$residuals # as.numeric(residuals(fit, standardize = TRUE))
    ljung.box <- Box.test(resid, lag = 20, type = "Ljung-Box", fitdf = 0)
    p.val[i+1] <- ljung.box$p.value
  }
}

dates <- EURUSD[, 1] 
forecasts.ts <- xts(forecasts, dates[(window.length):length(returns)])

# create lagged series of forecasts and sign of forecast
ag.forecasts <- Lag(forecasts.ts, 1)
ag.direction <- ifelse(ag.forecasts > 0, 1, ifelse(ag.forecasts < 0, -1, 0))

# Create the ARIMA/GARCH returns for the directional system
ag.direction.returns <- ag.direction * returns[(window.length):length(returns)]
ag.direction.returns[1] <- 0 # remove NA

# Create the backtests for ARIMA/GARCH and Buy & Hold
ag.curve <- log( cumprod( 1 + ag.direction.returns) )
buy.hold.ts <- xts(returns[(window.length):length(returns)], dates[(window.length):length(returns)])
buy.hold.curve <- log(cumprod(1 + buy.hold.ts))
both.curves <- cbind(ag.curve, buy.hold.curve)
names(both.curves) <- c("Strategy returns", "Buy and hold returns")

# plot both curves together
myColors <- c( "darkorange", "blue") #"red", "darkgreen") #,, "darkviolet")
plot(x = both.curves[,"Strategy returns"], xlab = "Time", ylab = "Cumulative Return",
     main = "Cumulative Returns", ylim = c(-0.25, 0.4), major.ticks= "quarters",
     minor.ticks = FALSE, col = "darkorange")
lines(x = both.curves[,"Buy and hold returns"], col = "blue")
legend(x = 'bottomleft', legend = c("Strategy", "B&H"),
       lty = 1, col = myColors)


# Test entering a trade only when prediction exceeds a threshold magnitude
simp.forecasts <- exp(ag.forecasts) - 1
threshold <- 0.000025
ag.threshold <- ifelse(simp.forecasts > threshold, 1, ifelse(simp.forecasts < -threshold, -1, 0))
ag.threshold.returns <- ag.threshold * returns[(window.length):length(returns)]
ag.threshold.returns[1] <- 0 # remove NA
ag.threshold.curve <- log(cumprod( 1 + ag.threshold.returns))
both.curves <- cbind(ag.threshold.curve, ag.curve)
names(both.curves) <- c("Filtered", "Unfiltered")

# plot both curves together
plot(x = both.curves[,"Filtered"], xlab = "Time", ylab = "Cumulative Return",
     main = "Cumulative Returns",  major.ticks= "quarters", #
     minor.ticks = FALSE, ylim = c(-0.2, 0.45), col = "darkgreen")
lines(x = both.curves[,"Unfiltered"], col = "blue")
legend(x = 'bottomleft', legend = c("Filtered", "Unfiltered"),
       lty = 1, col = c('darkgreen', 'blue'))

# Test entering a trade only when prediction and p.val exceeds a threshold magnitude
simp.forecasts <- exp(ag.forecasts) - 1
threshold <- 0.000025
p.vals.ts <- xts(p.val, dates[(window.length):length(returns)])
ag.pvals <-  Lag(p.val, 1)
p.threshold <- 0.05

ag.filtered <- ifelse((simp.forecasts > threshold & ag.pvals > p.threshold), 1, 
                      ifelse((simp.forecasts < -threshold & ag.pvals > p.threshold), -1, 0))
ag.filtered.returns <- ag.filtered * returns[(window.length):length(returns)]
ag.filtered.returns[1] <- 0 # remove NA
ag.filtered.curve <- log(cumprod( 1 + ag.filtered.returns))
all.curves <- cbind(ag.filtered.curve, ag.threshold.curve, ag.curve)
names(all.curves) <- c("Filtered-magnitude and p-val", "Filtered-magnitude", "Unfiltered")
# plot three curves together
plot(x = all.curves[,"Filtered-magnitude and p-val"], xlab = "Time", ylab = "Cumulative Return",
     main = "Cumulative Returns",  major.ticks= "quarters", #
     minor.ticks = FALSE, ylim = c(-0.2, 0.45), col = "deeppink2")
lines(x = all.curves[,"Filtered-magnitude"], col = "darkgreen")
lines(x = all.curves[, "Unfiltered"], col = "blue")
legend(x = 'bottomleft', legend = c("Filtered-magnitude and p-val", "Filtered-magnitude", "Unfiltered"),
       lty = 1, col = c("deeppink2", "darkgreen", "blue"))