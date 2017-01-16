# http://r-analytics.blogspot.ru/2012/03/t.html#.WGQBrRug-Um
d.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
mean(d.intake)

# Вопрос: отличается ли это выборочное среднее значение от установленной нормы в 7725 кДж/сутки? 
# Разница между нашим выборочным значением и этим нормативом довольно прилична: 7725 - 6753.6 = 971.4. 
# Но насколько велика эта разница статистически? Ответить на этот вопрос поможет одновыборочный t-тест. 
# Как и другие варианты t-теста, одновыборочный тест Стьюдента выполняется в R при помощи функции t.test()

t.test(d.intake, mu = 7725)

# Видим, что для имеющихся выборочных данных t-критерий составляет -2.821 при 10 степенях свободы (df). 
# Вероятность получить такое (либо большее) значение t при условии, что проверяемая нулевая гипотеза верна, 
# оказалась весьма мала: p-value = 0.01814  (во всяком случае, это меньше 5%). 
# Следовательно (см. выше), мы можем отклонить проверяемую нулевую гипотезу о равенстве выборочного 
# среднего значения нормативу и принять альтернативную гипотезу 
# (alternative hypothesis: true mean is not equal to 7725). Делая это, мы рискуем ошибиться с вероятностью менее 5%.


tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = mean)
tapply(X = mtcars$disp, INDEX = list(mtcars$am, mtcars$vs), FUN = mean)  # для нескольких факторов


SE <- function(x) {sd(x)/sqrt(length(x))}
tapply(X = mtcars$disp, INDEX = mtcars$am, FUN = SE)

################################################################################
# Протокол разведочного анализа данных: проверка однородности групповых дисперсий
# Помимо графических способов, в R имеются также функции для формальной проверки нулевой гипотезы о равенстве дисперсий в k группах:
#http://r-analytics.blogspot.ru/2012/06/blog-post.html#.WGaILxug-Uk

# bartlett.test() - выполняет тест Бартлетта.
bartlett.test(count ~ spray, data = InsectSprays)

# fligner.test() - тест Флигнера:
fligner.test(count ~ spray, data = InsectSprays)

# leveneTest() из пакета car: выполняет тест Левене:
library(car)
attach(Moore)
leveneTest(conformity, interaction(fcategory, partner.status))
################################################################################

# http://r-analytics.blogspot.ru/2012/06/blog-post_14.html#.WGaOxRug-Uk
# проверка на нормальность распределения


################################################################################
# 30 июня 2012
# Протокол разведочного анализа данных: выявление коллинеарности
# http://r-analytics.blogspot.ru/2012/07/blog-post.html#.WGjYihug-Uk

# Функции для автоматического расчета VIF и выполнения перечиселенных выше шагов реализованы в нескольких пакетах для R. 
# Одним из примеров таких функций является vif() из пакета car.


# https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/
require(MASS)
require(clusterGeneration)

set.seed(2)
num.vars <- 15
num.obs <- 200
cov.mat <- genPositiveDefMat(num.vars, covMethod="unifcorrmat")$Sigma
rand.vars <- mvrnorm(num.obs, rep(0, num.vars), Sigma=cov.mat)

parms <- runif(num.vars, -10, 10)
y <- rand.vars %*% matrix(parms) + rnorm(num.obs, sd=20)


lm.dat <- data.frame(y, rand.vars)
form.in <- paste('y ~', paste(names(lm.dat)[-1], collapse='+'))
mod1 <- lm(form.in, data=lm.dat)
summary(mod1)


vif_func<-function(in_frame, thresh=10, trace=T, ...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  # get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init, collab=c('var','vif'), rowlab=rep('', nrow(vif_init)), quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    return(names(in_dat))
  }
}


vif_func(in_frame=rand.vars, thresh=5, trace=T)

keep.dat <- vif_func(in_frame=rand.vars, thresh=5, trace=F)
form.in <- paste('y ~', paste(keep.dat, collapse='+'))
mod2 <- lm(form.in, data=lm.dat)
summary(mod2)


# Кабаков
# pause on each graph
par(ask = TRUE)
# save current graphical parameters
opar <- par(no.readonly = TRUE)
library(car)

fit <- lm(weight ~ height, data = women)
summary(fit)
women$weight
fitted(fit)  # предсказанные значения согласно модели 247 (Кабаков)
residuals(fit) # остатки модели
plot(women$height, women$weight, main = "Women Age 30-39", 
     xlab = "Height (in inches)", ylab = "Weight (in pounds)")
# add the line of best fit
abline(fit)

# Listing 8.2 - Polynomial regression
fit2 <- lm(weight ~ height + I(height^2), data = women)
summary(fit2)

plot(women$height, women$weight, main = "Women Age 30-39", 
     xlab = "Height (in inches)", ylab = "Weight (in lbs)")
lines(women$height, fitted(fit2))

# scatterplot for women data

library(car) # 252
scatterplot(weight ~ height, data = women, spread = FALSE, 
            lty.smooth = 2, pch = 19, main = "Women Age 30-39", xlab = "Height (inches)", 
            ylab = "Weight (lbs.)")


# Listing 8.3 - Examining bivariate relationship
states <- as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy", "Income", "Frost")])
cor(states)

library(car)
scatterplotMatrix(states, spread = FALSE, lty.smooth = 2, main = "Scatterplot Matrix")

# Listing 8.4 - Multiple linear regression
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)

# Listing 8.5 Multiple linear regression with a significant
# interaction term
fit <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit)
library(effects)
plot(effect("hp:wt", fit, list(wt = c(2.2, 3.2, 4.2))), multiline = TRUE)

# simple regression diagnostics
fit <- lm(weight ~ height, data = women)
par(mfrow = c(2, 2))
plot(fit)
par(opar)
# regression diagnostics for quadratic fit
newfit <- lm(weight ~ height + I(height^2), data = women)
par(mfrow = c(2, 2))
plot(newfit)
par(opar)

newfit <- lm(weight ~ height + I(height^2), data = women[-c(13, 15),])
par(mfrow = c(2, 2))
plot(newfit)
par(opar)



fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
par(mfrow = c(2, 2))
plot(fit)
par(opar)

# УСОВЕРШЕНСТВОВАННЫЙ ПОДХОД
# Assessing normality - НОРМАЛЬНОСТЬ
library(car)
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
qqPlot(fit, labels = FALSE, simulate = TRUE, main = "Q-Q Plot")

# 268
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE, xlab="Studentized Residual", main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)), add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y, col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(fit)

# НЕЗАВИСИМОСТЬ ОСТАТКОВ
durbinWatsonTest(fit)

# ЛИНЕЙНОСТЬ
crPlots(fit, one.page = TRUE, ask = FALSE)

# ГОМОСКЕДАНТИЧНОСТЬ
ncvTest(fit)
# Результат теста НЕЗНАЧИМ p = 0.1863156 - условие однородности дисперсии остатков ВЫПОЛНЕНО
spreadLevelPlot(fit)


library(gvlma)  # 273
gvmodel <- gvlma(fit)
summary(gvmodel)

# МУЛЬТИКОЛЛИНЕАРНОСТЬ
vif(fit)
sqrt(vif(fit)) > 2

####################################################################################
# НЕОБЫЧНЫЕ НАБЛЮДЕНИЯ
# ВЫБРОСЫ
outlierTest(fit)


# ТОЧКИ ВЫСОКОЙ НАПРЯЖЕННОСТИ 275 
# это выбросы в отношении других независимых переменных
hat.plot <- function(fit){
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2, 3) * p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)


# ВЛИЯТЕЛЬНЫЕ НАБЛЮДЕНИЯ
cutoff <- 4/(nrow(states) - length(fit$coefficients) - 2)
#cutoff <- 1
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

# диаграммы добавленных переменных
avPlots(fit, ask = FALSE, onepage = TRUE, id.method = "identify")

influencePlot(fit, id.method = "identify", main = "Influence Plot", sub = "Circle size is proportial to Cook's Distance")


# СРАВНЕНИЕ МОДЕЛЕЙ
# Listing 8.11 - Comparing nested models using the anova function
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
anova(fit2, fit1)
# 0.9939 - результат теста незначим - считаем что удаление Income и Frost из модели НЕ УХУДШАЮТ МОДЕЛЬ


# Listing 8.12 - Comparing models with the Akaike Information Criterion
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
AIC(fit1, fit2)

# ОТБОР ПЕРЕМЕННЫХ
library(MASS)
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
stepAIC(fit, direction = "backward")  # Регрессия с пошаговым исключением переменных - СПОРНЫЙ СПОСОБ

# РЕГРЕССИЯ ПО ВСЕМ ПОДМНОЖЕСТВАМ
library(leaps)
leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data = states, nbest = 4)
plot(leaps, scale = "adjr2")

library(car)
subsets(leaps, statistic = "cp", main = "Cp Plot for All Subsets Regression")
abline(1, 1, lty = 2, col = "red")

# ОТНОСИТЕЛЬНАЯ ВАЖНОСТЬ ПРЕДИКТОРОВ 295
zstates <- as.data.frame(scale(states))
zfit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data = zstates)
coef(zfit)




relweights <- function(fit, ...) {
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  
  # correlations between original predictors and new orthogonal variables
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda^2
  
  # regression coefficients of Y on orthogonal variables
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt/rsquare) * 100
  lbls <- names(fit$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  
  # plot results
  barplot(t(import), names.arg = lbls, ylab = "% of R-Square", 
          xlab = "Predictor Variables", main = "Relative Importance of Predictor Variables", 
          sub = paste("R-Square = ", round(rsquare, digits = 3)), 
          ...)
  return(import)
}

# using relweights()

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
relweights(fit, col = "lightgrey")





####################################################################################
# R in Action: Chapter 13 
data(Affairs, package = "AER")
summary(Affairs)
table(Affairs$affairs)

Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair, levels = c(0, 1), labels = c("No", "Yes"))
table(Affairs$ynaffair)

fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating, 
                data = Affairs, family = binomial())
summary(fit.full)

fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + rating, data = Affairs, family = binomial())
summary(fit.reduced)

# compare models
anova(fit.reduced, fit.full, test = "Chisq")  # 428







par(mfrow=c(2, 2))
plot(fit)

vif(mod1) # variance inflation factors 
sqrt(vif(mod1)) > 2 # problem?
