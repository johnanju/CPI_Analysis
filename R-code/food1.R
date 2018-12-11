# Determine whether your original time series needs any nonlinear transformation(s)
# such as logging and/or deflating and/or raising-to-some-power in order to be
# converted to a form where its local random variations are consistent over time and
# generally symmetric in appearance. 

qqnorm(food1)
qqline(food1)

boxplot(food1~cycle(food1));plot(aggregate.ts(food1))
par(mfrow = c(1,2))
acf(food1);pacf(food1)


plot(aggregate(food1))
foodsma1= SMA(food1,n=25)
plot(foodsma1)

#Stationary Food and Beverages

components.food1=decompose(food1)
fsadjusted = food1-components.food1$seasonal
food1stationary= diff(fsadjusted,differences=2)
adf.test(food1stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(food1stationary) #accept null hypothesis ie

# KPSS test:
#   Null Hypothesis: the process is trend-stationary
# Alternative Hypothesis: the process has a unit root 

# ADF test:
#   Null Hypothesis: the process has a unit-root ("difference stationary")
# Alternative Hypothesis: the process has no unit root. 
# It can mean either that the process is stationary, or trend stationary, depending on which version of the ADF test is used


#ACF and PACF of stationary food

par(mfrow = c(3,2))
acf(food1)
pacf(food1)
acf(fsadjusted)
pacf(fsadjusted)
acf(food1stationary); 
pacf(food1stationary)



#shows ACF cuts of after first one and Lag 3 and 4 significant and PACf has 2,3 and 4 significant 
#this means there is 2 or 3 p terms and there is very little coorelation after differencing by 2

fit1 <- arima(fsadjusted,c(1,1,0),seasonal=list(order=c(1,2,0),period=12))
a = summary(fit1)
fcast1 = forecast(fit1,h=30)
checkresiduals(fit1)

fit2 <- arima(fsadjusted,c(1,1,1),seasonal=list(order=c(1,2,1),period=12))
b = summary(fit2)
fcast2 = forecast(fit2,h=30)
checkresiduals(fit2)

fit3 <- arima(fsadjusted,c(0,1,0),seasonal=list(order=c(0,2,1),period=12))
c = summary(fit3)
fcast3 = forecast(fit3,h=30)
checkresiduals(fit3)

fit4 <- arima(fsadjusted,c(0,1,1),seasonal=list(order=c(0,2,1),period=12))
d = summary(fit4)
fcast4 = forecast(fit4,h=30)
checkresiduals(fit4)

fit5 <- arima(fsadjusted,c(1,1,0),seasonal=list(order=c(1,2,1),period=12))
e = summary(fit5)
fcast5 = forecast(fit5,h=30)
checkresiduals(fit5)
fit6 <- arima(fsadjusted,c(2,1,2),seasonal=list(order=c(1,2,1),period=12))
f = summary(fit6)
fcast6 = forecast(fit6,h=30)
checkresiduals(fit6)
fit7 <- arima(fsadjusted,c(2,1,2),seasonal=list(order=c(1,2,1),period=12))
g = summary(fit7)
fcast7 = forecast(fit7,h=30)
checkresiduals(fit7)
fit8 <- arima(fsadjusted,c(2,1,2),seasonal=list(order=c(1,2,1),period=12))
h = summary(fit8)
fcast8 = forecast(fit8,h=30)
checkresiduals(fit8)


aic=AIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8)
bic=BIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8)

#----Model Summary & comparisom
model_summary = data.frame(ME = c(a[1],b[1],c[1],d[1],e[1],f[1],g[1],h[1]),
                           RMSE =c(a[2],b[2],c[2],d[2],e[2],f[2],g[2],h[2]),
                           MAE=c(a[3],b[3],c[3],d[3],e[3],f[3],g[3],h[3]),
                           MAPE=c(a[5],b[5],c[5],d[5],e[5],f[5],g[5],h[5]),
                           df=c(aic$df),
                           AIC=c(aic$AIC),
                           BIC=c(bic$BIC)
)
rownames(model_summary)=c(fcast1$method,fcast2$method,fcast3$method,fcast4$method,fcast5$method,fcast6$method,fcast7$method,fcast8$method)
model_summary 

par(mfrow = c(3,2))
plot(fcast1);plot(fcast2);plot(fcast3);plot(fcast4);plot(fcast5)

#====Best model=fit3=======
tsdisplay(residuals(fit5),lag.max = 24)

model_forecast = forecast(fit5,h=30)
plot(model_forecast)

checkresiduals(fit1)

autofood1=auto.arima(food1,stepwise = FALSE, trace = TRUE) # Best model: ARIMA(3,1,2) 

#model need to be revisted as the residuals are significant , there is pattern , they are not random
# d=1
# D=2
# p1=1
# p2=0
# q1=1
# q2=0
# P1=1
# P2=0
# Q1=1
# Q2=0


# > model_summary
#                                 ME      RMSE        MAE       MAPE df         AIC        BIC
# ARIMA(1,1,0)(1,2,0)[12]  0.009462036 0.1682822 0.12693517 0.12294663  3 -16.7386439  -9.707228
# ARIMA(1,1,0)(1,2,1)[12]  0.008812988 0.1286936 0.09638728 0.09344055  4 -37.8926582 -28.517436
# ARIMA(1,1,0)(0,2,0)[12]  0.008683497 0.1923149 0.14189070 0.13697960  2  -0.8092837   3.878327
# ARIMA(1,1,0)(0,2,1)[12]  0.009031519 0.1342728 0.09840300 0.09540393  3 -36.9017823 -29.870366
# ARIMA(1,1,1)(1,2,0)[12]  0.009120250 0.1682010 0.12795411 0.12395867  4 -14.9962076  -5.620986
# ARIMA(1,1,1)(1,2,1)[12]  0.008783206 0.1287361 0.09663407 0.09368661  5 -35.9104912 -24.191464
# ARIMA(1,1,1)(0,2,0)[12]  0.007993384 0.1887752 0.14262698 0.13777803  3  -1.6318668   5.399550
# ARIMA(1,1,1)(0,2,1)[12]  0.008948568 0.1339595 0.09931922 0.09631039  4 -35.2354717 -25.860250
# ARIMA(0,1,0)(1,2,0)[12]  0.042512363 0.6177108 0.41086999 0.39267044  2 181.0774401 185.765051
# ARIMA(0,1,0)(1,2,1)[12] -0.001293657 0.4339210 0.30162629 0.28762742  3 153.3365287 160.367945
# ARIMA(0,1,0)(0,2,0)[12]  0.084776691 0.6954393 0.45232860 0.43229048  1 193.7951181 196.138924
# ARIMA(0,1,0)(0,2,1)[12]  0.018469995 0.4564255 0.31780463 0.30311792  2 154.3257800 159.013391
# ARIMA(0,1,1)(1,2,0)[12]  0.029212852 0.3701145 0.25934471 0.24931776  3 104.8853409 111.916757
# ARIMA(0,1,1)(1,2,1)[12]  0.002910908 0.2662681 0.19034039 0.18257192  4  79.6899998  89.065221
# ARIMA(0,1,1)(0,2,0)[12]  0.045593413 0.3966614 0.27678109 0.26632461  2 113.3873484 118.074959
# ARIMA(0,1,1)(0,2,1)[12]  0.011613380 0.2758143 0.19679444 0.18876486  3  79.5181460  86.549562
# > 


