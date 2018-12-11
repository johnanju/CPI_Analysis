#Exploratory analysis recreation
par(mfrow = c(1,2))
boxplot(restaurants1~cycle(restaurants1));plot(aggregate.ts(restaurants1))
par(mfrow = c(1,2))
acf(restaurants1);pacf(restaurants1)

#Stationary Food and Beverages

stlrestaurants1 = stl(restaurants1,"per")
plot(stlrestaurants1)
components.restaurants1=decompose(restaurants1)
resadjusted = restaurants1-components.restaurants1$seasonal
restaurants1stationary= diff(resadjusted,differences=4)
adf.test(restaurants1stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(restaurants1stationary)
plot(restaurants1stationary)

#ACF and PACF of stationary food

par(mfrow = c(1,2))
acf(restaurants1stationary); 
pacf(restaurants1stationary)


#Finding parameters for ARIMA . d=2 from the above results.

par(mfrow = c(1,2))
acf(restaurants1stationary); 
pacf(restaurants1stationary)
#shows ACF cuts of after first one and Lag 3 and 4 significant and PACf has 2,3 and 4 significant 
#this means there is 2 or 3 p terms and there is very little coorelation after differencing by 2

fit1 <- arima(restaurants1stationary,c(0,1,1),seasonal=list(order=c(1,0,0),period=12))
a = summary(fit1)
fcast1 = forecast(fit1,h=30)


fit2 <- arima(restaurants1stationary,c(0,1,2),seasonal=list(order=c(1,0,0),period=12))
b = summary(fit2)
fcast2 = forecast(fit2,h=30)

fit3 <- arima(restaurants1stationary,c(0,1,2),seasonal=list(order=c(1,0,2),period=12))
c = summary(fit3)
fcast3 = forecast(fit3,h=30)

fit4 <- arima(restaurants1stationary,c(2,1,2),seasonal=list(order=c(1,1,2),period=12))
d = summary(fit4)
fcast4 = forecast(fit4,h=30)

fit5 <- arima(restaurants1stationary,c(1,1,4),seasonal=list(order=c(1,0,1),period=12))
e = summary(fit5)
fcast5 = forecast(fit5,h=30)

aic=AIC(fit1,fit2,fit3,fit4,fit5)
bic=BIC(fit1,fit2,fit3,fit4,fit5)




#----Model Summary & comparisom
model_summary = data.frame(ME = c(a[1],b[1],c[1],d[1],e[1]),
                           RMSE =c(a[2],b[2],c[2],d[2],e[2]),
                           MAE=c(a[3],b[3],c[3],d[3],e[3]),
                           MAPE=c(a[5],b[5],c[5],d[5],e[5]),
                           df=c(aic$df),
                           AIC=c(aic$AIC),
                           BIC=c(bic$BIC)
)
rownames(model_summary)=c(fcast1$method,fcast2$method,fcast3$method,fcast4$method,fcast5$method)
model_summary 

#====Best model=fit5=======
tsdisplay(residuals(fit3),lag.max = 24)

model_forecast = forecast(fit5,h=30)
plot(model_forecast)

# ARIMA(0,2,2)(0,4,0)[12] 0.005893862 0.3442742 0.2137107 0.2255397  3 141.34590 147.1996


# d=2
# D=4
# p1=3
# p2=0
# q1=2
# q2=0
# P1=1
# P2=0
# Q1=2
# Q2=0

# > model_summary
# ME      RMSE       MAE      MAPE df       AIC      BIC
# ARIMA(3,2,0)(1,4,0)[12] 0.010092493 0.2331953 0.1494882 0.1583823  5 113.00070 122.7569
# ARIMA(3,2,0)(1,4,2)[12] 0.014919318 0.1640075 0.1078423 0.1147225  7  95.35826 109.0170
# ARIMA(3,2,0)(0,4,0)[12] 0.006446542 0.3416028 0.2109487 0.2227018  4 142.48928 150.2943
# ARIMA(3,2,0)(0,4,2)[12] 0.010232846 0.2117099 0.1334720 0.1416746  6 111.90361 123.6111
# ARIMA(3,2,2)(1,4,0)[12] 0.008503576 0.2432774 0.1529051 0.1619914  7 119.26146 132.9202
# ARIMA(3,2,2)(1,4,2)[12] 0.012708094 0.1571188 0.1031779 0.1098057  9  96.75974 114.3209
# ARIMA(3,2,2)(0,4,0)[12] 0.005710119 0.3359585 0.2093161 0.2210352  6 144.89356 156.6010
# ARIMA(3,2,2)(0,4,2)[12] 0.010702497 0.1977824 0.1251809 0.1331697  8 109.31202 124.9220
# ARIMA(0,2,0)(1,4,0)[12] 0.008946953 0.2727062 0.1775581 0.1877303  2 122.08201 125.9845
# ARIMA(0,2,0)(1,4,2)[12] 0.012324768 0.1872849 0.1221724 0.1297758  4 104.73188 112.5369
# ARIMA(0,2,0)(0,4,0)[12] 0.003937000 0.3766363 0.2255399 0.2380245  1 147.32455 149.2758
# ARIMA(0,2,0)(0,4,2)[12] 0.010356299 0.2254826 0.1411486 0.1498812  3 113.43753 119.2913
# ARIMA(0,2,2)(1,4,0)[12] 0.012746035 0.2339924 0.1513849 0.1603067  4 111.56237 119.3673
# ARIMA(0,2,2)(1,4,2)[12] 0.014702270 0.1658788 0.1092986 0.1162267  6  94.17986 105.8873
# ARIMA(0,2,2)(0,4,0)[12] 0.005893862 0.3442742 0.2137107 0.2255397  3 141.34590 147.1996
# ARIMA(0,2,2)(0,4,2)[12] 0.010269197 0.2138690 0.1351896 0.1434844  5 108.56675 118.3230
# > 