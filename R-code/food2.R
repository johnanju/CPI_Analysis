qqnorm(food2)
qqline(food2)

boxplot(food2~cycle(food2));plot(aggregate.ts(food2))
par(mfrow = c(1,2))
acf(food1);pacf(food1)


plot(aggregate(food2))
foodsma1= SMA(food2,n=25)
plot(foodsma1)

#Stationary Food and Beverages

components.food2=decompose(food2)
fs2adjusted = food2-components.food2$seasonal
food2stationary= diff(fs2adjusted,differences=2)
adf.test(food2stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(food2stationary) #accept null hypothesis ie

par(mfrow = c(3,2))
acf(food2)
pacf(food2)
acf(fs2adjusted)
pacf(fs2adjusted)
acf(food2stationary); 
pacf(food2stationary)


# checkresiduals(f15)
# checkresiduals(f10)

# d=2
# D=3
# p1=1
# p2=0
# q1=1
# q2=0
# P1=2
# P2=0
# Q1=1
# Q2=0



# > model_summary
# ME      RMSE        MAE       MAPE df       AIC      BIC
# ARIMA(1,2,0)(2,3,0)[12] 0.0079061537 0.1340789 0.10179695 0.10151625  4  3.613981 11.18126
# ARIMA(1,2,0)(2,3,1)[12] 0.0136586641 0.1222562 0.09517770 0.09486123  5  3.654564 13.11367
# ARIMA(1,2,0)(0,3,0)[12] 0.0024264445 0.2392119 0.17138623 0.17187290  2 37.755974 41.53961
# ARIMA(1,2,0)(0,3,1)[12] 0.0131563710 0.1565142 0.12070137 0.12052966  3 13.643930 19.31939
# ARIMA(1,2,1)(2,3,0)[12] 0.0082154736 0.1329370 0.09954223 0.09919318  5  4.826883 14.28598
# ARIMA(1,2,1)(2,3,1)[12] 0.0138411153 0.1218545 0.09343900 0.09305955  6  4.997241 16.34816
# ARIMA(1,2,1)(0,3,0)[12] 0.0044336949 0.2360854 0.16838382 0.16874687  3 38.334100 44.00956
# ARIMA(1,2,1)(0,3,1)[12] 0.0142423009 0.1559522 0.11923669 0.11902149  4 14.945130 22.51241
# ARIMA(0,2,0)(2,3,0)[12] 0.0069867999 0.1499540 0.11271751 0.11259979  3 10.312724 15.98818
# ARIMA(0,2,0)(2,3,1)[12] 0.0128058195 0.1370389 0.10652106 0.10640046  4 10.871411 18.43869
# ARIMA(0,2,0)(0,3,0)[12] 0.0005002049 0.2509558 0.17065499 0.17108059  1 40.802913 42.69473
# ARIMA(0,2,0)(0,3,1)[12] 0.0104848928 0.1636350 0.12191888 0.12176162  2 17.059044 20.84269
# ARIMA(0,2,1)(2,3,0)[12] 0.0085389480 0.1352585 0.10073113 0.10039803  4  3.871748 11.43903
# ARIMA(0,2,1)(2,3,1)[12] 0.0140763864 0.1254501 0.09544929 0.09508191  5  4.355839 13.81494
# ARIMA(0,2,1)(0,3,0)[12] 0.0046574489 0.2363735 0.16802572 0.16837187  2 36.443240 40.22688
# ARIMA(0,2,1)(0,3,1)[12] 0.0144261936 0.1566065 0.11855950 0.11832712  3 13.299303 18.97476
# > 