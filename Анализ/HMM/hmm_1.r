# http://gekkoquant.com/2014/09/07/hidden-markov-models-examples-in-r-part-3-of-4/
library('RHmm')  # Load HMM package
# Code based upon http://systematicinvestor.wordpress.com/2012/11/01/regime-detection/
bullMarketOne = rnorm( 100, 0.1/365, 0.05/sqrt(365) )
bearMarket  = rnorm( 100, -0.2/365, 0.15/sqrt(365))
bullMarketTwo = rnorm( 100, 0.15/365, 0.07/sqrt(365) )
true.states = c(rep(1,100),rep(2,100),rep(1,100))
returns = c( bullMarketOne, bearMarket, bullMarketTwo )

y=returns
ResFit = HMMFit(y, nStates=2)  # Fit a HMM with 2 states to the data
VitPath = viterbi(ResFit, y)  # Use the viterbi algorithm to find the most likely state path (of the training data)
fb = forwardBackward(ResFit, y)  # Forward-backward procedure, compute probabilities


# Plot probabilities and implied states
layout(1:3)
plot(cumsum(returns),ylab="Cumulative Market Return",type="l", main="Fake Market Data")
plot(VitPath$states, type='s', main='Implied States', xlab='', ylab='State')
matplot(fb$Gamma, type='l', main='Smoothed Probabilities', ylab='Probability')
legend(x='topright', c('Bear Market - State 2','Bull Market - State 1'),  fill=1:2, bty='n')


##################################################################################
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


##################################################################################
library(depmixS4)
data(speed)
mod <- depmix(list(rt~1, corr~1), data=speed, nstates=2,
              family=list(gaussian(), multinomial("identity")), ntimes=c(168,134,137))
hmmfit <- fit(mod, verbose = FALSE)

forwardbackward(hmmfit)$gamma
summary(hmmfit)
getpars(hmmfit)
HMMpost <- posterior(hmmfit) 
vit <- viterbi(hmmfit)

par(mfrow=c(4, 1))
plot(speed$rt, col="darkviolet", type="l", lwd=2)
plot(as.numeric(speed$corr), col="darkolivegreen4", type="l", lwd=2)
plot(HMMpost$state, col="darkorange1", type="l", lwd=2)
plot(vit$state, col="darkorange1", type="l", lwd=2)
par(mfrow=c(1, 1))



data(speed)
# 2-state model on rt and corr from speed data set
# with Pacc as covariate on the transition matrix
# ntimes is used to specify the lengths of 3 separate series
mod1 <- depmix(list(rt~1, corr~1), data=speed, transition=~Pacc, nstates=2,
               family=list(gaussian(), multinomial("identity")), ntimes=c(168,134,137))
fb <- forwardbackward(mod1)
all.equal(-sum(log(fb$sca)), fb$logLike)

vit <- viterbi(mod1)
vit









