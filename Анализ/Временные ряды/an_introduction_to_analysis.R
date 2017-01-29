library(tseries) 
library(forecast)

setwd("D:/R_project/Analysis of Financial Time Series 3rd Edition/code")
library(fBasics)
da=read.table("d-ibm3dx7008.txt", header=T) 
ibm=da[,2]
sibm=ibm*100
basicStats(sibm)

skewness(sibm)


libm=log(ibm+1)*100
t.test(libm) # Test mean being zero.

normalTest(libm, method='jb') # The result shows the normality for log-return is rejected.

###########################################################################################
da=read.table("m-gm3dx7508.txt", header=T)
gm=da[,2]
gm1=ts(gm, frequency=12, start=c(1975, 1))
par(mfcol=c(2,1))
plot(gm, type='l')
plot(gm1, type='l')
acf(gm, lag=24)
acf(gm1, lag=24)

###########################################################################################
# CHAPTER 2
da=read.table("m-ibm3dx2608.txt",header=T)
sibm=da[,2]
Box.test(sibm,lag=5, type='Ljung')
plot(sibm)
############

gnp=scan(file="dgnp82.txt")
gnp1=ts(gnp,frequency=4,start=c(1947,2))
plot(gnp1)
points(gnp1, pch='*')
m1=ar(gnp, method="mle")
m1$order  #  An AR(3) is selected based on AIC

m2=arima(gnp,order=c(3,0,0))
m2
sqrt(m2$sigma2) # Residual standard error
p1=c(1,-m2$coef[1:3])  # Characteristic equation
roots=polyroot(p1)  # Find solutions
roots
Mod(roots)
k=2*pi/acos(1.590253/1.913308)  # To compute average length of business cycles
k


# Selection Rule
gnp=scan(file='q-gnp4791.txt')
ord=ar(gnp, method="mle")
ord$aic
ord$order
###########
vw=read.table("m-ibm3dx2608.txt",header=T)[,3]
plot(vw)
acf(vw)
pacf(vw)
mo = auto.arima(vw, trace = T)
mo

m3=arima(vw,order=c(3,0,0))
AIC(m3)
sqrt(m3$sigma2)
plot(m3$residuals)
Box.test(m3$residuals, lag=12, type='Ljung') # нулевая гипотиза - автокорреляции нет
str(m3)
###########
library(fUnitRoots)
da=read.table("q-gdp4708.txt", header=T)
gdp=log(da[, 4])
m1=ar(diff(gdp), method="mle")
m1$order
m1
adfTest(gdp, lags=10, type=c("c"))
plot(gdp)


library(fUnitRoots)
da=read.table("d-sp55008.txt", header=T)
sp5=log(da[, 7])
m2=ar(diff(sp5), method="mle")
m2$order
adfTest(sp5, lags=2, type=("ct"))
adfTest(sp5, lags=15, type=("ct"))
plot(sp5)
###########

r1=read.table("w-gs1yr.txt",header=T)[,4]
r3=read.table("w-gs3yr.txt",header=T)[,4]
plot(r1)
lines(r3, col='red')

m1=lm(r3~r1)
par(mfrow=c(2, 2))
plot(m1)
summary(m1)
par(mfrow=c(1, 1))
plot(m1$residuals, type="l")
acf(m1$residuals, lag=36)

c1=diff(r1)
c3=diff(r3)
m2=lm(c3 ~ -1+c1)
par(mfrow=c(2, 2))
plot(m2)
summary(m2)
par(mfrow=c(1, 1))
summary(m2)
plot(m2$residuals)
acf(m2$residuals)
acf(m2$residuals, lag=36)
m3=arima(c3, order=c(0,0,1), xreg=c1, include.mean=F)
m3
rsq=(sum(c3^2)-sum(m3$residuals^2))/sum(c3^2)
rsq
###########

da=read.table("m-intc7308.txt", header=T)
plot(da)
intc=log(da[,2]+1)
Box.test(intc, lag=12, type='Ljung')
acf(intc)

###########
# fGarch
da=read.table("m-intc7308.txt", header=T)
library(fGarch)
intc=log(da[,2]+1)
plot(intc)
m1=garchFit(intc ~ garch(1 ,0), data=intc, trace=F)
plot(m1)
summary(m1) 
predict(m1, 5)

m2=garchFit(intc ~ garch(1, 1), data=intc, trace=F)
summary(m2)

m3=garchFit(intc ~ garch(1, 0), data=intc, trace=F, cond.dist='std')
summary(m3)

m4=garchFit(intc ~ garch(1, 0), data=intc, cond.dist='sstd', trace=F)
m5=garchFit(intc ~ arma(1, 0)+garch(1, 1), data=intc ,trace=F)

###########
library(fGarch)
sp5=scan(file='sp500.dat')
plot(sp5, type='l')
# fit an AR(3)+GARCH(1,1) model
m1=garchFit(~arma(3, 0)+garch(1, 1), data=sp5, trace=F)
summary(m1)

m2=garchFit(~garch(1, 1), data=sp5, trace=F, cond.dist="std")
summary(m2)
stresi=residuals(m2, standardize=T)
plot(stresi, type='l')
Box.test(stresi, 10, type='Ljung')
pred = predict(m2, 5)
plot(pred$meanForecast)

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_3/ch3data")
Gt=scan(file="m-GLBTs.txt")
Gtemp=ts(Gt, frequency=12, start=c(1880, 1))
plot(Gtemp, xlab='year', ylab='temperature', type='l')
acf(diff(Gt), lag=36)
pacf(diff(Gt), lag=36)
m1=arima(Gt, order=c(1, 1, 2))
m1
acf(m1$residuals, lag=36)
m1=arima(Gt, order=c(1, 1, 2), seasonal=list(order=c(0, 0, 1), period=24))
m1
tsdiag(m1, gof=36)


setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_4/ch4data")
da=read.table("m-intcsp7309.txt", header=T)
head(da)
intc=log(da$intc+1)
rtn=ts(intc, frequency=12, start=c(1973, 1))
plot(rtn, type="l", xlab="year", ylab="ln-rtn")
t.test(intc)
Box.test(intc, lag=12, type="Ljung")

par(mfcol=c(2, 1))
acf(intc, lag=24)
acf(abs(intc), lag=24)
Box.test(abs(intc), lag=12, type="Ljung")
par(mfcol=c(1, 1))

y=intc-mean(intc)
plot(y^2)
acf(y^2)
Box.test(y^2, lag=12, type='Ljung')
setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_4")
source("archTest.R")
plot(y)
archTest(y, 12)
###########################################################################################

setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_4/ch4data")
fx=read.table("d-useu9910.txt", header=T)
fxeu=log(fx$rate)
eu=diff(fxeu)
acf(eu^2)
pacf(eu^2)
Box.test(eu, lag=20, type='Ljung')
t.test(eu)
Box.test(eu^2, lag=20, type='Ljung')
setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_4")
source("archTest.R")
plot(y)
archTest(y, 20)

###########################################################################################
setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_4/ch4data")
library(fGarch)
da=read.table("m-intcsp7309.txt", header=T)
head(da)
intc=log(da$intc+1)
m1=garchFit(~1+garch(3, 0), data=intc, trace=F)  # Fit an ARCH(3) model Use subcommand "trace = F" to reduce the output.
summary(m1)

m2=garchFit(~1+garch(1, 0), data=intc, trace=F)
summary(m2)
resi=residuals(m2, standardize=T)
tdx=c(1:444)/12+1973
par(mfcol=c(2, 2))
plot(tdx, resi, xlab='year', ylab='stand-resi', type='l')
acf(resi, lag=20)
acf(resi^2, lag=20)
pacf(resi, lag=20)
pacf(resi^2, lag=20)
par(mfrow=c(1, 1))
plot(m2)

m3=garchFit(~1+garch(1,0), data=intc, trace=F, cond.dist="std")
summary(m3)

###########################################################################################
setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_4/ch4data")
library(fGarch)
m4=garchFit(~1+garch(1, 1), data=intc, trace=F)
summary(m4)
v1=volatility(m4)  # Obtain volatility
resi=residuals(m4, standardize=T)  # Standardized residuals
vol=ts(v1, frequency=12, start=c(1973, 1))
res=ts(resi, frequency=12, start=c(1973, 1))
par(mfcol=c(2, 1))  # Show volatility and residuals
plot(vol, xlab='year', ylab='volatility', type='l')
plot(res, xlab='year', ylab='st. resi', type='l')
par(mfcol=c(2, 2))  # Obtain ACF & PACF
acf(resi, lag=24)
pacf(resi, lag=24)
acf(resi^2, lag=24)
pacf(resi^2, lag=24)


# Obtain plot of predictive intervals
par(mfcol=c(1, 1))
upp=0.0113+2*v1
low=0.0113-2*v1
tdx=c(1:444)/12+1973
plot(tdx, intc, xlab='year', ylab='series', type='l', ylim=c(-0.6, 0.6))
lines(tdx, upp, lty=2, col='red')
lines(tdx, low, lty=2, col='red')
abline(h=c(0.0113))

# Student-t innovations
m5=garchFit(~1+garch(1, 1), data=intc, trace=F, cond.dist="std")
summary(m5)
v2=volatility(m5)

m6=garchFit(~1+garch(1, 1), data=intc, trace=F, cond.dist='sstd')
summary(m6)
v3=volatility(m6)

par(mfcol=c(3, 1))
plot(tdx, v1, xlab='year', ylab='volatility', type='l', ylim=c(0.06, 0.3))
title(main='(a) Gaussian')
plot(tdx, v2, xlab='year', ylab='volatility', type='l', ylim=c(0.06, 0.3))
title(main='(b) Student-t')
plot(tdx, v3, xlab='year', ylab='volatility', type='l', ylim=c(0.06, 0.3))
title(main='(c) Skew Student-t')
cor(cbind(v1, v2, v3))
par(mfcol=c(1, 1))

library(fBasics)
basicStats(intc)
tt= -0.5526/sqrt(6/444)  # Testing skewness of the data
tt
tt= (0.8717-1)/0.0629  # Testing skewness of the model.
tt
pv=2*pnorm(tt)  # Compute p-value
pv
plot(m6)  # Selection: 13


yt=intc-mean(intc)
plot(yt)
plot(yt^2)
adf.test(yt^2, alternative="stationary") # нулевая гипотеза - ряд НЕ стацаонарный For the adf.test, if p-value < 0.05 => stationary
m1=arima(yt^2, order=c(1, 0, 1))
m1
mean(intc)
fit=yt^2-m1$residuals
v3=volatility(m6)  # m6 is GARCH(1,1) with skew-t innovations.
cor(v3, sqrt(fit))


###########################################################################################
setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_4")
source("Egarch.R")  # Compile R script
da=read.table("ch4data/m-ibmsp6709.txt", header=T)  # Load data
dim(da)  # Check sample size of the data

ibm=log(da$ibm+1)  # Take log transformation
Box.test(ibm, lag=12, type='Ljung')  # Check serial correlations

m1=Egarch(ibm)
names(m1)
stresi=m1$residuals/m1$volatility  # Obtain standardized residuals
tdx=c(1:516)/12+1967  # Compute time index
par(mfcol=c(2, 1))  # Plotting
plot(tdx, ibm, xlab='year', ylab='logrtn', type='l')
plot(tdx, stresi, xlab='year', ylab='stresi', type='l')

Box.test(stresi, lag=10, type='Ljung')  # Model checking
Box.test(stresi, lag=20, type='Ljung')
Box.test(stresi^2, lag=10, type='Ljung')
Box.test(stresi^2, lag=20, type='Ljung')


###########################################################################################
# Tgarch
setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_4")
da=read.table("ch4data/d-useu9910.txt", header=T)
par(mfcol=c(1, 1))
plot(da$rate)
fx=log(da$rate)
eu=diff(fx)*100
source("Tgarch11.R")  # Compile R script
m1=Tgarch11(eu)
names(m1)
at=m1$residuals
sigt=m1$volatility
resi=at/sigt
Box.test(resi, lag=10, type='Ljung')
Box.test(resi, lag=20, type='Ljung')
Box.test(resi^2, lag=10, type='Ljung')
Box.test(resi^2, lag=20, type='Ljung')


m1=garchFit(~1+aparch(1, 1), data=eu, trace=F)
summary(m1)
m2=garchFit(~1+aparch(1, 1), data=eu, delta=2, include.delta=F, trace=F)
summary(m2)
v2=volatility(m2)  # Obtain volatility
resi=residuals(m2, standardize=T)  # Standardized residuals
vol=ts(v2, frequency=12, start=c(1973, 1))
res=ts(resi, frequency=12, start=c(1973, 1))
par(mfcol=c(2, 1))  # Show volatility and residuals
plot(vol, xlab='year', ylab='volatility', type='l')
plot(res, xlab='year', ylab='st. resi', type='l')
par(mfcol=c(1, 1))
plot(m2)

###########################################################################################
# NGARCH # Figure 4.18  228 страница
setwd("D:/R_project/An Introduction to Analysis of Financial Data with R/chapter_4")
da=read.table("ch4data/d-useu9910.txt", header=T)
fx=log(da$rate)
eu=diff(fx)*100
source("Ngarch.R")
m1=Ngarch(eu)
res=m1$residuals
vol=m1$volatility
resi=res/vol
Box.test(resi, lag=10, type='Ljung')  # Model checking
Box.test(resi, lag=20, type='Ljung')
Box.test(resi^2, lag=10, type='Ljung')
Box.test(resi^2, lag=20, type='Ljung')

par(mfcol=c(2, 1))  # Show volatility and residuals
plot(vol, xlab='year', ylab='volatility', type='l')
plot(res^2, xlab='year', ylab='st. resi', type='l')
par(mfcol=c(1, 1))

m2=garchFit(~1+garch(1, 1), data=eu, trace=F)
summary(m2)
res_2=residuals(m2, standardize=T)
vol_2=volatility(m2)
resi_2=res_2/vol_2

plot(vol, xlab='year', ylab='volatility', type='l')
lines(vol_2, col="red")

