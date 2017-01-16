library(TTR)
library(caret)
library(rminer)
library(deepnet)
# https://www.mql5.com/ru/articles/1103

pr.OHLC <- function (o, h, l, c) 
{
  #Объединим векторы котировок в матрицу, предварительно их развернув
  #Индексация векторов таймсерий в R начинается с 1. 
  #Направление индексации- от старых к новым.   
  price <- cbind(Open = rev(o), High = rev(h), Low = rev(l), Close = rev(c))
  Med <- (price[, 2] + price[, 3])/2
  CO <- price[, 4] - price[, 1]
  #добавим в матрицу Med и CO
  price <- cbind(price, Med, CO)
}

price_0_0 <- getSymbols("EURUSD", src="Finam", from='2000-01-01', to ='2014-12-31', auto.assign=FALSE)
price_0 <- as.data.frame(price_0_0)
head(price)
price <- cbind(Open=price_0$EURUSD.Open, High=price_0$EURUSD.High, Low=price_0$EURUSD.Low, Close=price_0$EURUSD.Close)
Med <- (price[, 2] + price[, 3])/2
CO <- price[, 4] - price[, 1]
#добавим в матрицу Med и CO
price <- cbind(price, Med, CO)
head(price)

adx<-ADX(price, n = 16)
plot.ts(head(adx, 200))
summary(adx)

ar<-aroon(price[ , c('High', 'Low')], n = 16)[ ,'oscillator']
plot(head(ar, 200), t = "l")
abline(h = 0)
summary(ar)

cci<-CCI(price[ ,2:4], n = 16)
plot.ts(head(cci, 200))
abline(h = 0)
summary(cci)

chv<-chaikinVolatility(price[ , 2:4], n = 16)
summary(chv)
plot(head(chv, 200), t = "l")
abline(h = 0)


cmo<-CMO(price[ ,'Med'], n = 16)
plot(head(cmo, 200), t = "l")
abline(h = 0)
summary(cmo)

macd<-MACD(price[ ,'Med'], 12, 26, 9)[ ,'macd']
plot(head(macd, 200), t = "l")
abline(h = 0)
summary(macd)

osma<-macd - MACD(price[ ,'Med'],12, 26, 9)[ ,'signal']
plot(head(osma, 200), t = "l")
abline(h = 0)
summary(osma)

rsi<-RSI(price[ ,'Med'], n = 16)
plot(head(rsi, 200), t = "l")
abline(h = 50)
summary(rsi)

stoh<-stoch(price[ ,2:4], 14, 3, 3)
plot.ts(head(stoh, 200))
summary(stoh)

smi<-SMI(price[ ,2:4],n = 13, nFast = 2, nSlow = 25, nSig = 9)
plot.ts(head(smi, 200))
summary(smi)

vol<-volatility(price[ ,1:4],n = 16,calc = "yang.zhang", N =96)
plot.ts(head(vol, 200))
summary(vol)

In<-function(p = 16){
  adx<-ADX(price, n = p);
  ar<-aroon(price[ ,c('High', 'Low')], n=p)[ ,'oscillator'];
  cci<-CCI(price[ ,2:4], n = p);
  chv<-chaikinVolatility(price[ ,2:4], n = p);
  cmo<-CMO(price[ ,'Med'], n = p);
  macd<-MACD(price[ ,'Med'], 12, 26, 9)[ ,'macd'];
  osma<-macd - MACD(price[ ,'Med'],12, 26, 9)[ ,'signal'];
  rsi<-RSI(price[ ,'Med'], n = p);
  stoh<-stoch(price[ ,2:4],14, 3, 3);
  smi<-SMI(price[ ,2:4],n = p, nFast = 2, nSlow = 25, nSig = 9);
  vol<-volatility(price[ ,1:4],n = p,calc="yang.zhang", N=96);
  In<-cbind(adx, ar, cci, chv, cmo, macd, osma, rsi, stoh, smi, vol);
  return(In)
}
X<-In()
tail(X)

Out<-function(ch=0.0037){
  # ЗигЗаг имеет значения (определен) на каждом баре а не только в вершинах 
  zz<-ZigZag(price[ ,'Med'], change = ch, percent = F, retrace = F, lastExtreme = T);
  n<-1:length(zz);
  # На последних барах неопределенные значения заменим на последние известные
  for(i in n) { if(is.na(zz[i])) zz[i] = zz[i-1];}
  #Определим скорость изменения ЗигЗага и сдвинем на один бар в будущее
  dz<-c(diff(zz), NA);
  #Если скорость >0 - сигнал = 0(Buy), если <0, сигнал = 1 (Sell) иначе NA
  sig<-ifelse(dz>0, 0, ifelse(dz<0, 1, NA));
  return(sig);
}

Y<-Out()
table(Y)

Clearing<-function(x, y){
  dt<-cbind(x,y);
  n<-ncol(dt)
  dt<-na.omit(dt)
  return(dt);  
}
dt<-Clearing(X,Y); nrow(dt)


Balancing<-function(DT){
  #Вычисляем таблицу с количеством классов
  cl<-table(DT[ ,ncol(DT)]);
  #Если разбаланс меньше 15%, возвращаем исходную матрицу
  if(max(cl)/min(cl)<= 1.15) return(DT)
  #Иначе балансируем в большую сторону
  DT<-if(max(cl)/min(cl)> 1.15){ 
    upSample(x = DT[ ,-ncol(DT)],y = as.factor(DT[ , ncol(DT)]), yname = "Y")
  }
  #Преобразуем у (фактор) в число
  DT$Y<-as.numeric(DT$Y)
  #Перекодируем у из 1,2 в 0,1
  DT$Y<-ifelse(DT$Y == 1, 0, 1)
  #Преобразуем датафрейм в матрицу
  DT<-as.matrix(DT)
  return(DT);
}

dt.b<-Balancing(dt)
x<-dt.b[ ,-ncol(dt.b)]
y<-dt.b[ , ncol(dt.b)]

t<-holdout(y, ratio = 8/10, mode = "random")

spSign<-preProcess(x[t$tr, ], method = "spatialSign")
x.tr<-predict(spSign, x[t$tr, ])
x.ts<-predict(spSign, x[t$ts, ])

# sae.dnn.train(x, y, hidden = c(10), activationfun = "sigm", learningrate = 0.8, momentum = 0.5, learningrate_scale = 1, output = "sigm", sae_output = "linear",
#               numepochs = 3, batchsize = 100, hidden_dropout = 0, visible_dropout = 0)

# х - матрица входных данных;
# y - вектор или матрица целевых переменных;
# hidden - вектор с числом нейронов в каждом скрытом слое. По умолчанию с(10);
# activationfun - функция активации скрытых нейронов. Может быть "sigm", "linear", "tanh". По умолчанию "sigm";
# learningrate - уровень обучения для градиентного спуска. По умолчанию = 0.8;
# momentum - момент для градиентного спуска. По умолчанию = 0.5;
# learningrate_scale - уровень обучения может быть умножен на эту величину после каждой итерации. По умолчанию =1.0;
# numepochs - количество итераций для обучения. По умолчанию =3;
# batchsize - размер мини порций на которых проводится обучение. По умолчанию =100;
# output - функция активации для выходных нейронов, может быть "sigm", "linear", "softmax". По умолчанию "sigm";
# sae_output - функция активации выходных нейронов SAE, может быть "sigm", "linear", "softmax". По умолчанию "linear";
# hidden_dropout - удаляемая часть для скрытых слоев. По умолчанию =0;
# visible_dropout - удаляемая часть видимого (входного) слоя. По умолчанию =0.

system.time(SAE<-sae.dnn.train(x= x.tr, y= y[t$tr], hidden=c(100, 100, 100), activationfun = "tanh", 
                               learningrate = 0.6, momentum = 0.5, learningrate_scale = 1.0, 
                               output = "sigm", sae_output = "linear", numepochs = 10, batchsize = 100, 
                               hidden_dropout = 0, visible_dropout = 0))

pr.sae<-nn.predict(SAE, x.ts);
summary(pr.sae)


pr<-ifelse(pr.sae>mean(pr.sae), 1, 0)
confusionMatrix(y[t$ts], pr)


new.x<-predict(spSign,tail(dt[ ,-ncol(dt)], 500))
pr.sae1<-nn.predict(SAE, new.x)
pr.sig<-ifelse(pr.sae1>mean(pr.sae1), -1, 1)
table(pr.sig)
new.y<-ifelse(tail(dt[  , ncol(dt)], 500) == 0, 1, -1)
table(new.y)
cm1<-confusionMatrix(new.y, pr.sig)
cm1


bal<-cumsum(tail(price[ , 'CO'], 500) * pr.sig)
plot(bal, t = "l")
abline(h = 0)

# Сравним с балансом, который бы получился по идеальным сигналам от ZZ. Красная линия - баланс по сигналам нейросети:
bal.zz<-cumsum(tail(price[ , 'CO'], 500) * new.y)
plot(bal.zz,  t = "l")
lines(bal,  col = 2)

Estimation<-function(X, Y, r = 8/10, m = "random", norm = "spatialSign",
                     h = c(10), act = "tanh", LR = 0.8, Mom = 0.5, 
                     out = "sigm", sae = "linear", Ep = 10, Bs = 50, 
                     CM=F){
  #Индексы тренировочного и тестового наборов
  t<-holdout(Y, ratio = r, mode = m)
  #Параметры препроцессинга
  prepr<-preProcess(X[t$tr,  ], method = norm)
  #Разделяем на train и test наборы с препроцессингом 
  x.tr<-predict(prepr, X[t$tr,  ])
  x.ts<-predict(prepr, X[t$ts,  ])
  y.tr<- Y[t$tr]; y.ts<- Y[t$ts]
  #Обучаем модель
  SAE<-sae.dnn.train(x = x.tr , y = y.tr , hidden = h, 
                     activationfun = act,
                     learningrate = LR, momentum = Mom, 
                     output = out, sae_output = sae, 
                     numepochs = Ep, batchsize = Bs)
  #Получаем предсказание по тестовому набору
  pr.sae<-nn.predict(SAE, x.ts)
  #Перекодируем его в сигналы 1,0
  pr<-ifelse(pr.sae>mean(pr.sae), 1, 0)
  #Вычисляем коэффициент Accuracy или ошибку классификации
  if(CM) err<-unname(confusionMatrix(y.ts, pr)$overall[1])
  if(!CM) err<-nn.test(SAE, x.ts, y.ts, mean(pr.sae))
  return(err)
}

Err<-Estimation(X = dt[ ,-ncol(dt)], Y = dt[ ,ncol(dt)], h=c(30, 30, 30), LR= 0.7)
Err

Testing<-function(dt1, dt2, r=8/10, m = "random", norm = "spatialSign",
                  h = c(10), act = "tanh", LR = 0.8, Mom = 0.5, 
                  out = "sigm", sae = "linear", Ep = 10, Bs=50, 
                  pr = T, bar = 500){
  X<-dt1[  ,-ncol(dt1)]
  Y<-dt1[  ,ncol(dt1)]
  t<-holdout(Y,  ratio = r,  mode = m)
  prepr<-preProcess(X[t$tr,  ], method = norm)
  x.tr<-predict(prepr, X[t$tr,  ])
  y.tr<- Y[t$tr]; 
  SAE<-sae.dnn.train(x = x.tr , y = y.tr , hidden = h, 
                     activationfun = act,
                     learningrate = LR, momentum = Mom, 
                     output = out, sae_output = sae, 
                     numepochs = Ep, batchsize = Bs)
  X<-dt2[ ,-ncol(dt2)]
  Y<-dt2[ ,ncol(dt2)]
  x.ts<-predict(prepr, tail(X, bar))
  y.ts<-tail(Y, bar)
  pr.sae<-nn.predict(SAE, x.ts)
  sig<-ifelse(pr.sae>mean(pr.sae), -1, 1)
  sig.zz<-ifelse(y.ts == 0, 1,-1 )
  bal<-cumsum(tail(price[  ,'CO'], bar) * sig)
  bal.zz<-cumsum(tail(price[  ,'CO'], bar) * sig.zz)
  if(pr) return(bal)
  if(!pr) return(bal.zz)
}
Bal<-Testing(dt.b, dt, h=c(30, 30, 30), LR= 0.7)
plot(Bal, t = "l")
abline(h = 0)


plot(tail(price[  ,'Close'], 500), t = "l")
abline(v = c(50,450), col=2)

Testing.1<-function(dt1, dt2, r = 8/10, m = "random", norm = "spatialSign",
                    h = c(10), act = "tanh", LR = 0.8, Mom = 0.5, 
                    out = "sigm", sae = "linear", Ep = 10, Bs = 50, 
                    pr = T, bar = 500, dec=1){
  X<-dt1[ ,-ncol(dt1)]
  Y<-dt1[ ,ncol(dt1)]
  t<-holdout(Y, ratio = r, mode = m)
  prepr<-preProcess(X[t$tr, ], method = norm)
  x.tr<-predict(prepr, X[t$tr, ])
  y.tr<- Y[t$tr]; 
  SAE<-sae.dnn.train(x = x.tr , y = y.tr , hidden = h, 
                     activationfun = act,
                     learningrate = LR, momentum = Mom, 
                     output = out, sae_output = sae, 
                     numepochs = Ep, batchsize = Bs)
  X<-dt2[ ,-ncol(dt2)]
  Y<-dt2[ ,ncol(dt2)]
  x.ts<-predict(prepr, tail(X, bar))
  y.ts<-tail(Y, bar)
  pr.sae<-nn.predict(SAE, x.ts)
  #Вариант +/- mean
  if(dec == 1) sig<-ifelse(pr.sae>mean(pr.sae), -1, 1)
  #Вариант 60/40
  if(dec == 2) sig<-ifelse(pr.sae>0.6, -1, ifelse(pr.sae<0.4, 1, 0))
  sig.zz<-ifelse(y.ts == 0, 1,-1 )
  bal<-cumsum(tail(price[  ,'CO'], bar) * sig)
  bal.zz<-cumsum(tail(price[  ,'CO'], bar) * sig.zz)
  if(pr) return(bal)
  if(!pr) return(bal.zz)
}

set.seed<-1245
Bal1<-Testing.1(dt.b, dt, h = c(30, 30, 30), LR = 0.7, dec = 1)

set.seed<-1245
Bal2<-Testing.1(dt.b, dt, h = c(30, 30, 30), LR = 0.7, dec = 2)
plot(Bal2, t = "l")
lines(Bal1, col = 2)



