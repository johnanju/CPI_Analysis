#Exploratory analysis recreation
par(mfrow = c(1,2))
boxplot(recreation1~cycle(recreation1));plot(aggregate.ts(recreation1))
par(mfrow = c(1,2))
acf(recreation1);pacf(recreation1)

#Stationary Food and Beverages

stlrecreation1 = stl(recreation1,"per")
plot(stlrecreation1)
components.recreation1=decompose(recreation1)
recadjusted = recreation1-components.recreation1$seasonal
recreation1stationary= diff(recadjusted,differences=2)
adf.test(recreation1stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(recreation1stationary)
plot(recreation1stationary)


#ACF and PACF of stationary food

par(mfrow = c(1,2))
acf(recreation1stationary); 
pacf(recreation1stationary)


#Finding parameters for ARIMA . d=2 from the above results.

par(mfrow = c(1,2))
acf(recreation1stationary); 
pacf(recreation1stationary)
#shows ACF cuts of after first one and Lag 3 and 4 significant and PACf has 2,3 and 4 significant 
#this means there is 2 or 3 p terms and there is very little coorelation after differencing by 2

fit1 <- arima(recreation1stationary,c(1,2,0),seasonal=list(order=c(1,2,0),period=12))
a = summary(fit1)
fcast1 = forecast(fit1,h=30)


fit2 <- arima(recreation1stationary,c(1,2,1),seasonal=list(order=c(1,2,1),period=12))
b = summary(fit2)
fcast2 = forecast(fit2,h=30)

fit3 <- arima(recreation1stationary,c(1,2,2),seasonal=list(order=c(1,2,2),period=12))
c = summary(fit3)
fcast3 = forecast(fit3,h=30)

fit4 <- arima(recreation1stationary,c(2,2,2),seasonal=list(order=c(1,2,2),period=12))
d = summary(fit4)
fcast4 = forecast(fit4,h=30)

fit5 <- arima(recreation1stationary,c(1,2,2),seasonal=list(order=c(1,2,1),period=12))
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
tsdisplay(residuals(fit5),lag.max = 24)

model_forecast = forecast(fit5,h=30)
plot(model_forecast)



# d=2
# D=4
# p1=1
# p2=0
# q1=1
# q2=0
# P1=1
# P2=0
# Q1=1
# Q2=0

# > model_summary
# ME      RMSE        MAE       MAPE df       AIC       BIC
# ARIMA(1,2,0)(1,4,0)[12]  0.009506025 0.2292005 0.13442700 0.14298679  3 117.32912 123.18285
# ARIMA(1,2,0)(1,4,1)[12]  0.010655195 0.2182345 0.13085754 0.13944733  4 113.32963 121.13460
# ARIMA(1,2,0)(0,4,0)[12] -0.002362155 0.5399074 0.30625927 0.32454429  2 186.60087 190.50336
# ARIMA(1,2,0)(0,4,1)[12]  0.010110499 0.3350335 0.19226374 0.20501582  3 147.76293 153.61666
# ARIMA(1,2,1)(1,4,0)[12]  0.009564858 0.2371863 0.14126666 0.15046470  4 119.68062 127.48559
# ARIMA(1,2,1)(1,4,1)[12]  0.024070349 0.1933018 0.11988389 0.12787385  5 105.01766 114.77388
# ARIMA(2,2,1)(2,4,0)[12]  0.018707435 0.1362327 0.09041682 0.09683185  6  86.10068  97.80814
# ARIMA(1,2,1)(0,4,1)[12]  0.010631630 0.3047781 0.17677526 0.18863213  4 146.51218 154.31716
# ARIMA(0,2,0)(1,4,0)[12]  0.009653970 0.2303734 0.13963510 0.14848212  2 116.37814 120.28063
# ARIMA(0,2,0)(1,4,1)[12]  0.010630178 0.2206486 0.13558691 0.14447650  3 112.73880 118.59253
# ARIMA(0,2,0)(0,4,0)[12] -0.003432126 0.5646566 0.34222540 0.36264614  1 189.60617 191.55741
# ARIMA(0,2,0)(0,4,1)[12]  0.009351807 0.3461699 0.20888918 0.22282675  2 149.65753 153.56002
# ARIMA(0,2,1)(1,4,0)[12]  0.010624911 0.2276418 0.13421141 0.14279638  3 116.10839 121.96212
# ARIMA(0,2,1)(1,4,1)[12]  0.011185578 0.2159317 0.12919612 0.13771725  4 112.03666 119.84164
# ARIMA(0,2,1)(0,4,0)[12]  0.006155557 0.5224752 0.30889120 0.32758633  2 182.68324 186.58572
# ARIMA(0,2,1)(0,4,1)[12]  0.016989854 0.3275740 0.19369408 0.20666337  3 144.72788 150.58161
