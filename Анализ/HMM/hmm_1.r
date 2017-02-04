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

#################
trst <- c(0.9, 0.1, 0, 0, 0.1, 0.9, 0, 0)
mod <- depmix(list(rt ~ 1,corr ~ 1), data = speed, transition = ~ Pacc,
                   nstates = 2, family = list(gaussian(), multinomial("identity")),
                   trstart = trst, instart = c(0.99, 0.01))
fm1 <- fit(mod,verbose = FALSE, emc=em.control(rand=FALSE))
setpars(mod, value = 1:npar(mod))
setpars(mod, getpars(mod, which = "fixed"))
getpars(fm1)
pars <- c(unlist(getpars(fm1)))
pars[6] <- pars[10] <- 11
pars[1] <- 0
pars[2] <- 1
pars[13] <- pars[14] <- 0.5
fm1 <- setpars(mod, pars)
conpat <- c(0, 0, rep(c(0, 1), 4), 1, 1, 0, 0, 1, 1, 1, 1)
conpat[6] <- conpat[10] <- 2
fm2 <- fit(fm1, equal = conpat)



data("balance")
set.seed(1)
mod <- mix(list(d1 ~ 1, d2 ~ 1, d3 ~ 1, d4 ~ 1), data = balance,
           nstates = 3, family = list(multinomial("identity"), multinomial("identity"), 
                                      multinomial("identity"), multinomial("identity")), 
           respstart = runif(24), prior = ~ age, initdata = balance)
fm <- fit(mod, verbose = FALSE, emc=em.control(rand=FALSE))
getpars(fm)
summary(fm, which = "prior")

# 4. Extending depmixS4
setClass("exgaus", contains="response")
library("gamlss")
library("gamlss.dist")
setGeneric("exgaus", function(y, pstart = NULL, fixed = NULL, ...)
standardGeneric("exgaus"))

setMethod("exgaus", signature(y = "ANY"), 
          function(y, pstart = NULL, fixed = NULL, ...){
            y <- matrix(y, length(y))
            x <- matrix(1)
            parameters <- list()
            npar <- 3
            if(is.null(fixed)) fixed <- as.logical(rep(0, npar))
            if(!is.null(pstart)) {
              if(length(pstart) != npar) stop("length of 'pstart' must be ", npar)
                  parameters$mu <- pstart[1]
                  parameters$sigma <- log(pstart[2])
                  parameters$nu <- log(pstart[3])
            }
            mod <- new("exgaus", parameters = parameters, fixed = fixed, x = x, y = y, npar = npar)
            mod
          }
)
# The fit method is defined as follows:
setMethod("fit", "exgaus",
          function(object, w) {
            if(missing(w)) w <- NULL
            y <- object@y
            fit <- gamlss(y ~ 1, weights = w, family = exGAUS(), 
                          control = gamlss.control(n.cyc = 100, trace = FALSE),
                          mu.start = object@parameters$mu,
                          sigma.start = exp(object@parameters$sigma),
                          nu.start = exp(object@parameters$nu))
            pars <- c(fit$mu.coefficients, fit$sigma.coefficients, fit$nu.coefficients)
            object <- setpars(object, pars)
            object
            }
)

rModels <- list()
rModels[[1]] <- list()
rModels[[1]][[1]] <- exgaus(speed$rt, pstart = c(5, 0.1, 0.1))
rModels[[1]][[2]] <- GLMresponse(formula = corr ~ 1, data = speed, family = multinomial(), pstart = c(0.5, 0.5))
rModels[[2]] <- list()
rModels[[2]][[1]] <- exgaus(speed$rt, pstart = c(6, 0.1, 0.1))
rModels[[2]][[2]] <- GLMresponse(formula = corr ~ 1, data = speed, family = multinomial(), pstart = c(0.1, 0.9))

trstart <- c(0.9, 0.1, 0.1, 0.9)
transition <- list()
transition[[1]] <- transInit(~ Pacc, nst = 2, data = speed, pstart = c(0.9, 0.1, 0, 0))
transition[[2]] <- transInit(~ Pacc, nst = 2, data = speed, pstart = c(0.1, 0.9, 0, 0))
inMod <- transInit(~ 1, ns = 2, pstart = c(0.1, 0.9), data = data.frame(1))

# Finally, we put everything together using makeDepmix and fit the model:
mod <- makeDepmix(response = rModels, transition = transition, prior = inMod, homogeneous = FALSE)
fm <- fit(mod, verbose = FALSE, emc=em.control(rand=FALSE))


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
##################################################################################
require(depmixS4)
require(quantmod)
library(PerformanceAnalytics)
library(xts)
getSymbols('SPY', from = '2013-01-01', src='yahoo', adjust = TRUE)
spyRets <- na.omit(Return.calculate(Ad(SPY)))

set.seed(123)
hmm <- depmix(SPY.Adjusted ~ 1, family = gaussian(), nstates = 3, data=spyRets)
hmmfit <- fit(hmm, verbose = FALSE)
post_probs <- posterior(hmmfit)
post_probs <- xts(post_probs, order.by=index(spyRets))

par(mfrow=c(3, 1))
plot(SPY$SPY.Close, col="darkviolet", type="l", lwd=2)
plot(post_probs$state, col="darkorange1", type="l", lwd=2)
plot(spyRets$SPY.Adjusted, col="darkolivegreen4", type="l", lwd=2)
par(mfrow=c(1, 1))

summaryMat <- data.frame(summary(hmmfit))
colnames(summaryMat) <- c("Intercept", "SD")
bullState <- which(summaryMat$Intercept > 0)
bearState <- which(summaryMat$Intercept < 0)

hmmRets <- spyRets * lag(post_probs$state == 3) - spyRets * lag(post_probs$state == 1)
charts.PerformanceSummary(hmmRets)
table.AnnualizedReturns(hmmRets)

require(doMC)
# install.packages("doMC", repos="http://R-Forge.R-project.org")
dailyHMM <- function(data, nPoints) {
  subRets <- data[1:nPoints,]
  hmm <- depmix(SPY.Adjusted ~ 1, family = gaussian(), nstates = 3, data = subRets)
  hmmfit <- fit(hmm, verbose = FALSE)
  post_probs <- posterior(hmmfit)
  summaryMat <- data.frame(summary(hmmfit))
  colnames(summaryMat) <- c("Intercept", "SD")
  bullState <- which(summaryMat$Intercept > 0)
  bearState <- which(summaryMat$Intercept < 0)
  if(last(post_probs$state) %in% bullState) {
    state <- xts(1, order.by=last(index(subRets)))
  } else if (last(post_probs$state) %in% bearState) {
    state <- xts(-1, order.by=last(index(subRets)))
  } else {
    state <- xts(0, order.by=last(index(subRets)))
  }
  colnames(state) <- "State"
  return(state)
}

# took 3 hours in parallel
t1 <- Sys.time()
set.seed(123)
registerDoMC((detectCores() - 1))
states <- foreach(i = 500:nrow(spyRets), .combine=rbind) %dopar% {
  dailyHMM(data = spyRets, nPoints = i)
}
t2 <- Sys.time()
print(t2-t1)




