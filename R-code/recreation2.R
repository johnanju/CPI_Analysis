qqnorm(recreation2)
qqline(recreation2)

boxplot(recreation2~cycle(recreation2));plot(aggregate.ts(recreation2))
par(mfrow = c(1,2))
acf(recreation2);pacf(recreation2)


plot(aggregate(recreation2))

#Stationary Food and Beverages

components.recreation2=decompose(recreation2)
rec2adjusted = recreation2-components.recreation2$seasonal
rec2stationary= diff(rec2adjusted,differences=1)
adf.test(rec2stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(rec2stationary) #accept null hypothesis ie

par(mfrow = c(3,2))
acf(recreation2)
pacf(recreation2)
acf(res2adjusted)
pacf(res2adjusted)
acf(recreation2stationary); 
pacf(recreation2stationary)

# 
# d=2
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
# ME      RMSE        MAE       MAPE df        AIC        BIC
# ARIMA(1,2,0)(1,3,0)[12]  0.0014922105 0.1357834 0.10076929 0.10079119  3  -6.358890 -0.6834289
# ARIMA(1,2,0)(1,3,1)[12]  0.0093387650 0.1237205 0.09065835 0.09086638  4  -8.790702 -1.2234203
# ARIMA(1,2,0)(0,3,0)[12] -0.0030650921 0.2194975 0.15775342 0.15662361  2  29.451002 33.2346429
# ARIMA(1,2,0)(0,3,1)[12]  0.0086844158 0.1405306 0.10178644 0.10179259  3   1.827291  7.5027521
# ARIMA(1,2,1)(1,3,0)[12]  0.0029566737 0.1306572 0.09664063 0.09673574  4  -9.322836 -1.7555545
# ARIMA(1,2,1)(1,3,1)[12]  0.0091739282 0.1200572 0.08814539 0.08838263  5 -11.222980 -1.7638783
# ARIMA(1,2,1)(0,3,0)[12]  0.0041389388 0.2008259 0.14179930 0.14087412  3  22.586053 28.2615139
# ARIMA(1,2,1)(0,3,1)[12]  0.0097262392 0.1356953 0.10006298 0.10008864  4  -2.339433  5.2278479
# ARIMA(0,2,0)(1,3,0)[12] -0.0005896457 0.1698930 0.12010407 0.11962163  2  13.905898 17.6895388
# ARIMA(0,2,0)(1,3,1)[12]  0.0097169580 0.1320900 0.09548740 0.09541822  3   5.085177 10.7606374
# ARIMA(0,2,0)(0,3,0)[12] -0.0039464195 0.2619572 0.18854301 0.18638031  1  45.384644 47.2764640
# ARIMA(0,2,0)(0,3,1)[12]  0.0066081151 0.1623728 0.12252636 0.12197071  2  16.751992 20.5356322
# ARIMA(0,2,1)(1,3,0)[12]  0.0033629574 0.1343122 0.09958657 0.09964898  3  -9.194773 -3.5193118
# ARIMA(0,2,1)(1,3,1)[12]  0.0094796842 0.1202459 0.08862975 0.08886525  4 -12.406238 -4.8389572
# ARIMA(0,2,1)(0,3,0)[12]  0.0081741088 0.1979173 0.14028774 0.13932617  2  21.203103 24.9867435
# ARIMA(0,2,1)(0,3,1)[12]  0.0098083683 0.1360889 0.10033321 0.10036985  3  -3.970124  1.7053368
# > 