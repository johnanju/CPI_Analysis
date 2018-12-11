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

plot(tsData)

timeseriesseasonallyadjusted <- tsData- components.tsData$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=2)
plot(tsstationary)
plot(aggregate.ts(tsData))
#Exploratory analysis food 
par(mfrow = c(1,2))
boxplot(goods2~cycle(goods2));plot(goods2)
par(mfrow = c(1,2))
acf(goods2);pacf(goods2)

#Stationary Food and Beverages

stlgoods2 = stl(goods2,"per")
plot(stlgoods2)
components.goods2=decompose(goods2)
g2adjusted = goods2-components.goods2$seasonal
acf(g2adjusted)


goods2stationary= diff(goods2,differences=3)
plot(goods2stationary)
acf(goods2stationary)
goods2stationary =fsadjusted
adf.test(goods2stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(goods2stationary) #accept null hypothesis ie

# KPSS test:
#   Null Hypothesis: the process is trend-stationary
# Alternative Hypothesis: the process has a unit root 

# ADF test:
#   Null Hypothesis: the process has a unit-root ("difference stationary")
# Alternative Hypothesis: the process has no unit root. 
# It can mean either that the process is stationary, or trend stationary, depending on which version of the ADF test is used





#ACF and PACF of stationary food

par(mfrow = c(2,2))
acf(goods2)
pacf(goods2)
acf(goods2stationary); 
pacf(goods2stationary)

#shows ACF cuts of after first one and Lag 3 and 4 significant and PACf has 2,3 and 4 significant 
#this means there is 2 or 3 p terms and there is very little coorelation after differencing by 2

fit1 <- arima(goods2,c(0,0,0),seasonal=list(order=c(1,0,0),period=12))
a = summary(fit1)
fcast1 = forecast(fit1,h=30)


fit2 <- arima(goods2,c(0,1,2),seasonal=list(order=c(0,0,0),period=12))
b = summary(fit2)
fcast2 = forecast(fit2,h=30)

fit3 <- arima(goods2,c(0,1,1),seasonal=list(order=c(0,1,0),period=12))
c = summary(fit3)
fcast3 = forecast(fit3,h=30)

fit4 <- arima(goods2,c(0,0,0),seasonal=list(order=c(1,2,1),period=12))
d = summary(fit4)
fcast4 = forecast(fit4,h=30)

fit5 <- arima(goods2,c(0,0,0),seasonal=list(order=c(1,1,2),period=12))
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

m=HoltWinters(goods2,seasonal = c("additive"))
plot(m)
f= predict(m,n.head=12,prediction.interval = T,level = 0.95)
plot(m,f)




# ARIMA(3,0,0)(1,0,0)[12] with non-zero mean  2.981402e-03 0.06726742 0.05417075 0.05448563  6 -200.94861 -186.1532
# ARIMA(3,0,0)(1,0,1)[12] with non-zero mean  1.994366e-03 0.05729246 0.04773080 0.04798969  7 -210.97626 -193.7149
# ARIMA(3,0,0) with non-zero mean             3.107879e-03 0.06832682 0.05521271 0.05553965  5 -200.70648 -188.3769
# ARIMA(3,0,0)(0,0,1)[12] with non-zero mean  2.517357e-03 0.06535917 0.05326441 0.05354827  6 -203.76092 -188.9655
# ARIMA(3,0,1)(1,0,0)[12] with non-zero mean  2.701737e-03 0.06691033 0.05447690 0.05478546  7 -199.75745 -182.4961
# ARIMA(1,3,1)(1,2,1)[12]                     2.852348e-02 0.11704060 0.08315865 0.08347824  5  -30.45542  -19.9837
# ARIMA(3,0,1) with non-zero mean            -5.707574e-03 0.07656489 0.06214008 0.06246119  6 -180.45316 -165.6577
# ARIMA(3,0,1)(0,0,1)[12] with non-zero mean  4.441415e-03 0.07106664 0.05890563 0.05924358  7 -185.05198 -167.7906
# ARIMA(0,0,0)(1,0,0)[12] with non-zero mean -3.239191e-02 1.20150594 0.94666186 0.95915993  3  285.07815  292.4759
# ARIMA(0,0,0)(1,0,1)[12] with non-zero mean -1.135484e-01 0.97638976 0.73648180 0.74655254  4  272.74956  282.6132
# ARIMA(0,0,0) with non-zero mean             1.388408e-14 1.20683294 0.95354996 0.96570942  2  283.60722  288.5390
# ARIMA(0,0,0)(0,0,1)[12] with non-zero mean -1.327795e-01 0.96060025 0.73092706 0.74070749  3  271.20764  278.6054
# ARIMA(0,0,1)(1,0,0)[12] with non-zero mean  8.267920e-03 0.62975662 0.49067086 0.49692204  4  178.97167  188.8353
# ARIMA(0,0,1)(1,0,1)[12] with non-zero mean  5.271087e-02 0.59122923 0.49298606 0.49839094  5  174.96231  187.2919
# ARIMA(0,0,1) with non-zero mean             1.187561e-02 0.63005904 0.49127857 0.49748610  3  176.99472  184.3924
# ARIMA(0,0,1)(0,0,1)[12] with non-zero mean  6.477849e-02 0.58880722 0.49654426 0.50162555  4  173.78619  183.6498
# > 


     