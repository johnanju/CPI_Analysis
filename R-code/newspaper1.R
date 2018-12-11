

qqnorm(newspaper1)
qqline(newspaper1)
#Exploratory analysis recreation
par(mfrow = c(1,2))
boxplot(newspaper1~cycle(newspaper1));plot(aggregate.ts(newspaper1))
par(mfrow = c(1,2))
acf(newspaper1);pacf(newspaper1)

#Stationary Food and Beverages

stlnewspaper1 = stl(newspaper1,"per")
plot(stlnewspaper1)
components.newspaper1=decompose(newspaper1)
newsseasonal = newspaper1-components.newspaper1$seasonal # seasonal componenet removed

newsp=diff(newspaper1,differences=2) # first difference and seasonality removed
compo=decompose(newsp)
newsdifferenced=newspaper1-compo$seasonal 

lognewspaper=log(newspaper1) #log transformed
compo.lognewspaper=decompose(lognewspaper) 
newslogadjusted= newspaper1- compo.lognewspaper$seasonal # seasonality removed from log
newsstationary= diff(newslogadjusted,differences=2) # differenced seasonal log

newspaper1stationary= diff(newsseasonal,differences=2)
adf.test(newspaper1stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(newspaper1stationary)

plot(newspaper1stationary)


#ACF and PACF of stationary food

par(mfrow = c(1,2))
acf(newspaper1stationary); 
pacf(newspaper1stationary)

#Finding parameters for ARIMA . d=2 from the above results.

#shows ACF cuts of after first one and Lag 3 and 4 significant and PACf has 2,3 and 4 significant 
#this means there is 2 or 3 p terms and there is very little coorelation after differencing by 2

fit1 <- arima(rsadjusted,c(2,2,2),seasonal=list(order=c(3,2,0),period=12))
a = summary(fit1)
fcast1 = forecast(fit1,h=30)
fit2 <- arima(rsadjusted,c(1,2,1),seasonal=list(order=c(1,2,0),period=12))
b = summary(fit2)
fcast2 = forecast(fit2,h=30)
fit3 <- arima(rsadjusted,c(1,2,2),seasonal=list(order=c(1,2,1),period=12))
c = summary(fit3)
fcast3 = forecast(fit3,h=30)
fit4 <- arima(rsadjusted,c(2,2,1),seasonal=list(order=c(1,2,1),period=12))
d = summary(fit4)
fcast4 = forecast(fit4,h=30)
fit5 <- arima(rsadjusted,c(1,2,1),seasonal=list(order=c(2,2,0),period=12))
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
                           BIC=c(bic$BIC))
rownames(model_summary)=c(fcast1$method,fcast2$method,fcast3$method,fcast4$method,fcast5$method)
model_summary 
# ME      RMSE       MAE      MAPE df      AIC      BIC
# ARIMA(2,3,1)(2,3,0)[12] -0.001219309 0.1947292 0.1279962 0.1420392  6 53.25646 66.11527
# ARIMA(1,2,1)(1,3,0)[12]  0.022429102 0.2343211 0.1428968 0.1588876  4 56.79819 65.43373
# ARIMA(1,2,2)(1,3,1)[12]  0.016550869 0.1624071 0.1100940 0.1229016  6 35.77510 48.72840
# ARIMA(2,3,1)(1,3,1)[12] -0.000873021 0.1558547 0.1023502 0.1139521  6 43.03790 55.89671
# ARIMA(1,3,1)(2,3,0)[12] -0.002014350 0.2024149 0.1336879 0.1483284  5 55.19181 65.90749
autonewspaper1=auto.arima(newspaper1,stepwise = FALSE, trace = TRUE) # Best model: ARIMA(2,2,0)(0,0,1)[12]  

#====Best model=fit5=======
tsdisplay(residuals(fit3),lag.max = 24)

model_forecast = forecast(fit5,h=30)
plot(model_forecast)


# d=1
# D=2
# p1=1
# p2=0
# q1=1
# q2=0
# P1=1
# P2=0
# Q1=1
# Q2=0  # ARIMA(0,1,0)(0,2,0)[12] 0.0219293810 0.2970244 0.20475924 0.22846071  1  62.5558294  64.899635

# d=2
# D=2
# p1=1
# p2=0
# q1=1
# q2=0
# P1=1
# P2=0
# Q1=1
# Q2=0 ARIMA(0,2,0)(0,2,0)[12]  0.0013256702 0.2950972 0.19930063 0.22306518  1  56.9707859  59.301519



# 
# model_summary
# ME      RMSE        MAE       MAPE df         AIC        BIC
# ARIMA(1,2,0)(1,2,0)[12]  0.0025050177 0.1604381 0.11061013 0.12431721  3 -20.1415003 -13.149300
# ARIMA(1,2,0)(1,2,1)[12] -0.0063593237 0.1137932 0.08010900 0.09062328  4 -47.2205699 -37.897636
# ARIMA(1,2,0)(0,2,0)[12]  0.0023198501 0.2543844 0.17410600 0.19523787  2  36.4772899  41.138757
# ARIMA(1,2,0)(0,2,1)[12] -0.0026751784 0.1514902 0.10527727 0.11872891  3 -18.7741617 -11.781962
# ARIMA(1,2,1)(1,2,0)[12]  0.0045385322 0.1550466 0.10391152 0.11681202  4 -23.2403987 -13.917465
# ARIMA(1,2,1)(1,2,1)[12] -0.0057546549 0.1108090 0.07686539 0.08696514  5 -49.6857925 -38.032126
# ARIMA(1,2,1)(0,2,0)[12]  0.0034907088 0.2498262 0.17100264 0.19149971  3  35.7728483  42.765048
# ARIMA(1,2,1)(0,2,1)[12] -0.0013073093 0.1480040 0.10266841 0.11570288  4 -20.4091927 -11.086259
# ARIMA(0,2,0)(1,2,0)[12]  0.0006428299 0.1731345 0.11955819 0.13410519  2  -8.2981060  -3.636639
# ARIMA(0,2,0)(1,2,1)[12] -0.0082513264 0.1254235 0.08887516 0.10026491  3 -32.0557848 -25.063585
# ARIMA(0,2,0)(0,2,0)[12]  0.0013256702 0.2950972 0.19930063 0.22306518  1  56.9707859  59.301519
# ARIMA(0,2,0)(0,2,1)[12] -0.0053359863 0.1737029 0.11909627 0.13401019  2   0.6827594   5.344226
# ARIMA(0,2,1)(1,2,0)[12]  0.0045340835 0.1550174 0.10347916 0.11631409  3 -25.2034400 -18.211240
# ARIMA(0,2,1)(1,2,1)[12] -0.0061169084 0.1110929 0.07761812 0.08773316  4 -51.1792255 -41.856292
# ARIMA(0,2,1)(0,2,0)[12]  0.0043465378 0.2514483 0.17007847 0.19036767  2  34.7530980  39.414565
# ARIMA(0,2,1)(0,2,1)[12] -0.0013183745 0.1489090 0.10254665 0.11550264  3 -21.4619204 -14.469720

