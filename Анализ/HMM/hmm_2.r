# https://www.quantstart.com/articles/hidden-markov-models-for-regime-detection-using-r
library(quantmod)
library(depmixS4)
getSymbols( "^GSPC", from="2004-01-01" )
gspcRets = diff( log( Cl( GSPC ) ) )
returns = as.numeric(gspcRets)
plot(gspcRets)

hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)
# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')


hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)
# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')


###################################################################################################
# Import the necessary packages and set 
# random seed for replication consistency
# install.packages('depmixS4')
# install.packages('quantmod')
library('depmixS4')
library('quantmod')
set.seed(1)

# Create the parameters for the bull and 
# bear market returns distributions
Nk_lower <- 50
Nk_upper <- 150
bull_mean <- 0.1
bull_var <- 0.1
bear_mean <- -0.05
bear_var <- 0.2

# Create the list of durations (in days) for each regime
days <- replicate(5, sample(Nk_lower:Nk_upper, 1))

# Create the various bull and bear markets returns
market_bull_1 <- rnorm( days[1], bull_mean, bull_var ) 
market_bear_2 <- rnorm( days[2], bear_mean, bear_var ) 
market_bull_3 <- rnorm( days[3], bull_mean, bull_var ) 
market_bear_4 <- rnorm( days[4], bear_mean, bear_var ) 
market_bull_5 <- rnorm( days[5], bull_mean, bull_var ) 

# Create the list of true regime states and full returns list
true_regimes <- c( rep(1,days[1]), rep(2,days[2]), rep(1,days[3]), rep(2,days[4]), rep(1,days[5]))
returns <- c( market_bull_1, market_bear_2, market_bull_3, market_bear_4, market_bull_5)

# Create and fit the Hidden Markov Model
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

# Output both the true regimes and the 
# posterior probabilities of the regimes
layout(1:2)
plot(post_probs$state, type='s', main='True Regimes', xlab='', ylab='Regime')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='topright', c('Bull','Bear'), fill=1:2, bty='n')

# Obtain S&P500 data from 2004 onwards and
# create the returns stream from this
getSymbols( "^GSPC", from="2004-01-01" )
gspcRets = diff( log( Cl( GSPC ) ) )
returns = as.numeric(gspcRets)

# Fit a Hidden Markov Model with two states 
# to the S&P500 returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='topright', c('Regime #1','Regime #2'), fill=1:2, bty='n')

# Fit a Hidden Markov Model with three states 
# to the S&P500 returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')



###################################################################################################
#http://systematicinvestor.github.io/Regime-Detection-Update
data <- new.env()
getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
price = Cl(data$SPY)
open = Op(data$SPY)
ret = diff(log(price))
ret = log(price) - log(open)

atr = ATR(HLC(data$SPY))[, 'atr']
#*****************************************************************
# Construct and fit a regime switching model
#****************************************************************** 
library(depmixS4)
library(xts)
library(quantmod)
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)


index = 14:nrow(price)
temp = data.frame(ret=as.vector(ret), atr=as.vector(atr))
temp = temp[index,]

# We're setting the LogReturns and ATR as our response variables, 
# want to set 4 different regimes, and setting the response distributions to be gaussian.
mod = depmix(list(ret~1, atr~1), data=temp, nstates=4, family=list(gaussian(), gaussian())) 
fm2 = fit(mod, verbose = FALSE)

print(summary(fm2))
probs = posterior(fm2)
print(head(probs))

layout(1:3)
plota(ret, type='h', col='blue')
plota.legend('S&P 500 Daily Log Returns', 'blue')
plota(atr, type='l', col='darkgreen')
plota.legend('Daily ATR(14)', 'darkgreen')

temp = NA * price
temp[index] = probs$state
plota(temp, type='l', col='darkred')
plota.legend('Market Regimes', 'darkred')

layout(1:4)
for(i in 2:5) {
  temp = NA * price
  temp[index] = probs[, i]
  plota(temp, type='l', col=i)
  plota.legend(paste('Market Regime', colnames(probs)[i]), i)
}


