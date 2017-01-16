library(forecast)
library(tseries)  # Библиотека для работы с временными рядами.
library(car)
library(lmtest)
set.seed(1)
x <- rnorm(1000)  # Сгенерировали стационарный ряд (1000 случайных чисел, распределённых по нормальному закону).
adf.test(x, alternative="stationary")  # Тест ADF.

# Теперь проверим заведомо нестационарный ряд
y <- diffinv(x)  # Нестационарный ряд.
adf.test(y, alternative="stationary")

# install.packages("rusquant", repos="http://R-Forge.R-project.org")
library(rusquant)  # Библиотека для скачивания котировок с сайта finam.ru.
ts1 <- getSymbols("EURUSD", src="Finam", from='2014-01-01', to ='2014-12-31', auto.assign=FALSE)
head(ts1)                                    # Вывели 5 первых строк.
adf.test(ts1[,4], alternative="stationary")  # Тест ADF.
# Дневные цены закрытия валютной пары EUR/USD можно считать нестационарным временным рядом 
# (гипотеза о наличии единичного корня не опровергнута).

# Теперь проверим ряд относительных приращений дневных цен закрытия той же самой валютной пары EUR/USD:
ts2 <- ROC(Cl(ts1), type="discrete")  # Относительные приращения цен закрытия.
head(ts2)                  # Вывели 5 первых строк; в первой строке видим NA.
ts2 <- ts2[-1,]                           # Удалили первую строку.
adf.test(ts2, alternative="stationary")   # Тест ADF.

# Вывод: гипотеза о наличии единичного корня опровергнута; ряд относительных приращений цен закрытия 
# валютной пары EUR/USD является стационарным. Таким образом, мы выяснили, что ряд цен закрытия 
# валютной пары EUR/USD является интегрированным 1-го порядка.




# https://talksonmarkets.files.wordpress.com/2012/09/time-series-analysis-with-arima-e28093-arch013.pdf
#ts3 <- na.omit(diff(log(ts1$EURUSD.Close)))
close_sp <- ts1$EURUSD.Close
tsdisplay(close_sp)
dy <- diff(close_sp)
tsdisplay(dy)

mod_1 <- Arima(close_sp, order = c(0, 1, 0))
summary(mod_1)
prognoz_1 <- forecast(mod_1, h=20)
plot(prognoz_1)

mod_a <- auto.arima(close_sp, trace = T)
prognoz_2 <- forecast(mod_a, h=20)
plot(prognoz_2)


ts3 <- na.omit(log(ts1$EURUSD.Close))
plot.zoo(ts3)
adf.test(ts3)  # стационарен

arima212 = arima(ts3, order=c(0,1,0))
summary(arima212)

resid <- arima212$residuals
par(mfrow=c(3, 1))
plot(resid)
acf(resid)
pacf(resid)

Box.test (resid, lag = 1, type = "Ljung")  # тест на автокорреляцию - автокорреляция для лага 1 не обнаружена

squared.resid <- resid^2
par(mfcol=c(3,1))
plot(squared.resid, main='Squared Residuals')
acf(squared.resid, main='ACF Squared Residuals', lag.max=100, ylim=c(-0.5,1))
pacf(squared.resid, main='PACF Squared Residuals',lag.max=100, ylim=c(-0.5,1))

arch08 <- garch(resid, order=c(0, 1), trace=T)
loglik08 <- logLik(arch08)
summary(arch08)



forecast212step1=forecast(arima212, 2, level=95)
plot(forecast212step1)
forecast212=forecast(arima212, 100, level=95)
plot(forecast212)


ht.arch08=arch08$fit[,1]^2 #use 1st column of fit
plot(ht.arch08, main='Conditional variances')


fit212 = fitted.values(arima212)
low = fit212 - 1.96*sqrt(ht.arch08)
high = fit212 + 1.96*sqrt(ht.arch08)
plot(ts3, type='l',main='Log Apple,Low,High')
lines(low, col='red')
lines(high, col='blue')

# par(mfrow=c(2, 1))
plot(coredata(ts3))
lines(low, col='red')
lines(high, col='blue')
lines(fit212, col="green")


archres = resid/sqrt(ht.arch08)
qqnorm(archres, main='ARIMA-ARCH Residuals')
qqline(archres)


archres = resid
qqnorm(archres, main='ARIMA-ARCH Residuals')
qqline(archres)

##############################################################################
# http://unstarched.net/wp-content/uploads/2013/06/an-example-in-rugarch.pdf
library(rugarch)
ls("package:rugarch")

model = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)

model2=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
  mean.model = list(armaOrder = c(2, 2), include.mean = TRUE),
  distribution.model = "sstd")
model2@model$pars

model=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm",
  fixed.pars = list(beta1=0.86))
model@model$pars

# 3 Getting data
data(sp500ret)
class(sp500ret)

sp500.prices=get.hist.quote(
  instrument = "^GSPC",
  quote = "Adj",
  provider = c("yahoo"), method = NULL,
  origin = "1899-12-30", compression = "d",
  retclass = c("zoo"), quiet = FALSE, drop = FALSE
)
sp500=as.data.frame(sp500.prices)
N=length(sp500[,1])
sp500.returns = 100*(log(sp500[2:N,])-log(sp500[1:(N-1),]))
plot(sp500.returns)

# 4 Estimating the model
modelfit = ugarchfit(spec=model,data=sp500ret)
modelfit
class(modelfit)
str(modelfit)

modelfit@fit$hessian
modelfit@fit$coef
modelfit@fit$fitted.values

coef(modelfit)
infocriteria(modelfit)
sigma(modelfit)
fitted(modelfit)
residuals(modelfit)

VaR=quantile(modelfit, 0.01)
# or in our case with standardized normal distribution we could do:
VaR=modelfit@fit$sigma*qnorm(0.01)+modelfit@fit$fitted.values
residuals_mod <- modelfit@fit$residuals
qqnorm(residuals_mod)
qqline(residuals_mod)
plot(VaR)

# 5 Forecasting
modelfor=ugarchforecast(modelfit, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0)
slotNames(modelfor)
#plot(modelfor)
slotNames(modelfor)
str(modelfor)

plot(modelfor@forecast$seriesFor)
modelfit@fit$coef["mu"]
uncmean(modelfit)

# 5.1 Rolling forecast
model=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"
)
modelfit=ugarchfit(model,data=sp500ret, out.sample=2)
length(sp500ret)
modelfit@model$modeldata$T

modelfor=ugarchforecast(modelfit, data = NULL, n.ahead = 1, n.roll = 2, out.sample = 2)
sigma(modelfor)
fitted(modelfor)

#Getting the expected return from the estimated model
mu = coef(modelfit)["mu"]
#Getting the relevant observed return from last
# two periods of sp500ret
return = sp500ret[5522:5523,]
#Getting residual of period T=5521 from modelfit
e5521 = as.vector(residuals(modelfit)[5521])
#Calculating residuals of period T=5522 and 5523
e5522=return[1]-mu
e5523=return[2]-mu
#Taking estimated parameters from modelfit
theta=coef(modelfit)

#Making function for forecast
.fgarch = function(e,sigma_0,theta) {
  omega=theta["omega"]
  alpha=theta["alpha1"]
  beta=theta["beta1"]
  sigma_1 = sqrt(omega + alpha*e^2 + beta*sigma_0^2)
  names(sigma_1)=NULL
  return(sigma_1)
}

#Getting estimated sigma for period T=5521 from modelfit
sigma5521=as.vector(sigma(modelfit)[5521])
#Forecast sigma_5522 and comparing with rugarch forecast
sigma5522=.fgarch(e5521, sigma_0=sigma5521, theta)
sigma5522
sigma(modelfor)[1]
#Forecast sigma_5523 and comparing with rugarch forecast
sigma5523=.fgarch(e5522,sigma_0=sigma5522,theta)
sigma5523
sigma(modelfor)[2]
#Forecast sigma_5523 and comparing with rugarch forecast
sigma5524=.fgarch(e5523,sigma_0=sigma5523,theta)
sigma5524
sigma(modelfor)[3]


qnorm(0.05)*sigma(modelfor)+coef(modelfit)["mu"]
# or using the built in function of the rugarchpackage:
quantile(modelfor,0.05)



# 6 Rolling forecast with reestimation
model=ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1)),
  distribution.model = "norm"
)
modelroll=ugarchroll(
  spec=model, data=sp500ret, n.ahead = 1, forecast.length = 100,
  n.start = NULL, refit.every = 50, refit.window = c("recursive"),
  window.size = NULL, solver = "hybrid", fit.control = list(),
  solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05),
  cluster = NULL, keep.coef = TRUE
)

slotNames(modelroll)
VaR=modelroll@forecast$VaR[,"alpha(1%)"]
return=modelroll@forecast$VaR[,"realized"]
Hit=return<VaR
sum(Hit)

q_st=qnorm(0.025)
sigma=modelroll@forecast$density[,"Sigma"]
mu= modelroll@forecast$density[,"Mu"]
Var_0025=q_st * sigma + mu

#####
# ARMA(1,1)-GARCH(1,1) model on the sp500ret dataset:
model=ugarchspec (
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1)),
  distribution.model = "sstd"
)
modelroll=ugarchroll (
  model, data=sp500ret, n.ahead = 1, forecast.length = 100,
  n.start = NULL, refit.every = 50, refit.window = c("recursive"),
  window.size = NULL, solver = "hybrid", fit.control = list(),
  solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05),
  cluster = NULL, keep.coef = TRUE
)
# getting the estimated skew from modelroll
skew.estimate=modelroll@forecast$density[,"Skew"]
# getting the estimated shape from modelroll
shape.estimate=modelroll@forecast$density[,"Shape"]
# using quantile funtion from rugarchpackage
# setting skew and shape according to estimates
# other values kept as specified in vignette
q_st=qdist(distribution="sstd",p=0.05,mu=0,
           sigma=1,lambda=-0.5,skew=skew.estimate,shape=shape.estimate)
mu=modelroll@forecast$density[,"Mu"]
sigma=modelroll@forecast$density[,"Sigma"]
VaR=sigma*q_st+mu





