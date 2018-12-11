# Determine whether your original time series needs any nonlinear transformation(s)
# such as logging and/or deflating and/or raising-to-some-power in order to be
# converted to a form where its local random variations are consistent over time and
# generally symmetric in appearance. 


lambda = BoxCox.lambda(goods1,lower = 0)
lambda

lambda = BoxCox.lambda(recreation1)
lambda

lambda = BoxCox.lambda(restaurants1)
lambda

lambda = BoxCox.lambda(newspaper1,lower=0)
lambda

x.transform = BoxCox.lambda(goods1,lambda)

lambda = BoxCox.lambda(goods1)
lambda



# Construction of an ARIMA model1.Stationarizethe series, if necessary, by differencing (& perhaps also logging, deflating, etc.)
# Study the pattern of autocorrelationsand partial autocorrelationsto determine if lags of the stationarized series and/or lags of the forecast errors should be included in the forecasting equation3.   
# Fit the model that is suggested and check its residual diagnostics, particularly the residual ACF and  PACF plots, to see if all coefficients are significant and all of the pattern has been explained.4.   
# Patterns that remain in the ACF and PACF may suggest the need for additional AR or MA terms

#ndiff says how many differencing the ts needs to make it stationary.

ndiffs(food1) #1
ndiffs(recreation1) #2
ndiffs(restaurants1) #2
ndiffs(newspaper1)v #2
ndiffs(goods1) #1


ndiffs(food2) #2
ndiffs(recreation2) #2
ndiffs(restaurants2) #2
ndiffs(newspaper2) #2
ndiffs(goods2) #0

timeseriesseasonallyadjusted <- tsData- components.tsData$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)
plot(tsstationary)
plot(aggregate.ts(tsData))
#Exploratory analysis food 
par(mfrow = c(1,2))
boxplot(goods1~cycle(goods1));plot(aggregate.ts(goods1))
par(mfrow = c(1,2))
acf(goods1);pacf(goods1)

#Stationary Food and Beverages

stlgoods1 = stl(goods1,"per")
plot(stlgoods1)
components.goods1=decompose(goods1)
gadjusted = goods1-components.goods1$seasonal

goods1stationary= diff(gadjusted,differences=1)
adf.test(goods1stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(goods1stationary) #accept null hypothesis ie

# KPSS test:
#   Null Hypothesis: the process is trend-stationary
# Alternative Hypothesis: the process has a unit root 

# ADF test:
#   Null Hypothesis: the process has a unit-root ("difference stationary")
# Alternative Hypothesis: the process has no unit root. 
# It can mean either that the process is stationary, or trend stationary, depending on which version of the ADF test is used





#ACF and PACF of stationary food

par(mfrow = c(2,2))
acf(goods1)
pacf(goods1)
acf(goods1stationary); 
pacf(goods1stationary)

#shows ACF cuts of after first one and Lag 3 and 4 significant and PACf has 2,3 and 4 significant 
#this means there is 2 or 3 p terms and there is very little coorelation after differencing by 2

fit1 <- arima(goods1,c(0,1,0),seasonal=list(order=c(0,0,0),period=12))
a = summary(fit1)
fcast1 = forecast(fit1,h=30)


fit2 <- arima(goods1,c(0,1,2),seasonal=list(order=c(0,0,0),period=12))
b = summary(fit2)
fcast2 = forecast(fit2,h=30)

fit3 <- arima(goods1,c(0,1,1),seasonal=list(order=c(0,1,0),period=12))
c = summary(fit3)
fcast3 = forecast(fit3,h=30)

fit4 <- arima(goods1,c(0,0,0),seasonal=list(order=c(1,2,1),period=12))
d = summary(fit4)
fcast4 = forecast(fit4,h=30)

fit5 <- arima(goods1,c(0,0,0),seasonal=list(order=c(1,1,2),period=12))
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
par(mfrow = c(3,2))
plot(fcast1);plot(fcast2);plot(fcast3);plot(fcast4);plot(fcast5)

#====Best model=fit3=======
tsdisplay(residuals(fit1),lag.max = 24)

model_forecast = forecast(fit3,h=30)
plot(model_forecast)

# 
# d=1
# D=3
# p1=1
# p2=0
# q1=1
# q2=0
# P1=1
# P2=0
# Q1=1
# Q2=0



# > model_summary
# ME      RMSE        MAE      MAPE df       AIC       BIC
# ARIMA(1,1,0)(1,3,0)[12]  0.0107785456 0.1970665 0.12420462 0.1383437  3  37.43615  43.95931
# ARIMA(1,1,0)(1,3,1)[12] -0.0073531319 0.1431041 0.09488036 0.1060472  4  21.31284  30.01038
# ARIMA(1,1,0)(0,3,0)[12]  0.0204638654 0.3540607 0.21363825 0.2372023  2  96.71634 101.06511
# ARIMA(1,1,0)(0,3,1)[12]  0.0024246762 0.2115138 0.13183672 0.1473709  3  51.78914  58.31230
# ARIMA(1,1,1)(1,3,0)[12]  0.0153027735 0.1942116 0.12049392 0.1341309  4  37.22958  45.92713
# ARIMA(1,1,1)(1,3,1)[12] -0.0005217078 0.1394181 0.09144562 0.1021033  5  18.94223  29.81416
# ARIMA(1,1,1)(0,3,0)[12]  0.0264984982 0.3401836 0.20387085 0.2263489  3  93.85482 100.37798
# ARIMA(1,1,1)(0,3,1)[12]  0.0090333881 0.2051531 0.12783391 0.1427864  4  49.36504  58.06259
# ARIMA(0,2,0)(1,3,0)[12]  0.0133944794 0.1984691 0.13022975 0.1452836  2  31.27021  35.58797
# ARIMA(0,2,0)(1,3,1)[12]  0.0124599994 0.1490134 0.10098482 0.1131080  3  17.61069  24.08734
# ARIMA(0,1,0)(0,3,0)[12]  0.0290677240 0.5896918 0.30465039 0.3311437  1 160.00413 162.17851
# ARIMA(0,1,0)(0,3,1)[12] -0.0211971141 0.3770067 0.20821534 0.2271793  2 123.98056 128.32934
# ARIMA(0,2,1)(1,3,0)[12]  0.0167506721 0.1943828 0.12191118 0.1359954  3  29.94683  36.42348
# ARIMA(0,2,1)(1,3,1)[12]  0.0158415949 0.1421771 0.09466776 0.1058819  4  14.51152  23.14705
# ARIMA(0,1,1)(0,3,0)[12]  0.0223322012 0.4654737 0.26970303 0.2960254  2 131.54504 135.89382
# ARIMA(0,1,1)(0,3,1)[12] -0.0115363475 0.2907325 0.18094868 0.1993662  3  92.60506  99.12823
# > 