# Determine whether your original time series needs any nonlinear transformation(s)
# such as logging and/or deflating and/or raising-to-some-power in order to be
# converted to a form where its local random variations are consistent over time and
# generally symmetric in appearance. 


lambda = BoxCox.lambda(food1,lower = 0)
lambda

lambda = BoxCox.lambda(recreation1)
lambda

lambda = BoxCox.lambda(restaurants1)
lambda

lambda = BoxCox.lambda(newspaper1,lower=0)
lambda

x.transform = BoxCox.lambda(food1,lambda)

lambda = BoxCox.lambda(food1)
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
boxplot(food1~cycle(food1));plot(aggregate.ts(food1))
par(mfrow = c(1,2))
acf(food1);pacf(food1)

#Stationary Food and Beverages

stlfood1 = stl(food1,"per")
plot(stlfood1)
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


plot(food1stationary)


#ACF and PACF of stationary food

par(mfrow = c(1,2))
acf(food1stationary); 
pacf(food1stationary)


#Finding parameters for ARIMA . d=2 from the above results.

par(mfrow = c(2,2))
acf(food1)
pacf(food1)
acf(food1stationary); 
pacf(food1stationary)


#shows ACF cuts of after first one and Lag 3 and 4 significant and PACf has 2,3 and 4 significant 
#this means there is 2 or 3 p terms and there is very little coorelation after differencing by 2

fit1 <- arima(food1stationary,c(1,2,0),seasonal=list(order=c(0,0,0),period=12))
a = summary(fit1)
fcast1 = forecast(fit1,h=30)
plot(fcast1)


fit2 <- arima(food1stationary,c(1,2,1),seasonal=list(order=c(1,2,1),period=12))
b = summary(fit2)
fcast2 = forecast(fit2,h=30)

fit3 <- arima(food1stationary,c(1,2,2),seasonal=list(order=c(1,2,2),period=12))
c = summary(fit3)
fcast3 = forecast(fit3,h=30)

fit4 <- arima(food1stationary,c(2,2,2),seasonal=list(order=c(1,2,2),period=12))
d = summary(fit4)
fcast4 = forecast(fit4,h=30)

fit5 <- arima(food1stationary,c(1,2,2),seasonal=list(order=c(1,2,1),period=12))
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


par(mfrow=c(2,3))
plot(fcast1)
plot(fcast2)
plot(fcast3)
plot(fcast4)
plot(fcast5)

#====Best model=fit3=======
tsdisplay(residuals(fit3),lag.max = 24)

model_forecast = forecast(fit3,h=30)
plot(model_forecast)

