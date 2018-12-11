qqnorm(restaurants2)
qqline(restaurants2)

boxplot(restaurants2~cycle(restaurants2));plot(aggregate.ts(restaurants2))
par(mfrow = c(1,2))
acf(restaurants2);pacf(restaurants2)


plot(aggregate(restaurants2))

#Stationary Food and Beverages

components.restaurants2=decompose(restaurants2)
res2adjusted = restaurants2-components.restaurants2$seasonal
res2stationary= diff(res2adjusted,differences=3)
adf.test(res2stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(res2stationary) #accept null hypothesis ie

par(mfrow = c(3,2))
acf(restaurants2)
pacf(restaurants2)
acf(res2adjusted)
pacf(res2adjusted)
acf(res2stationary); 
pacf(res2stationary)


# d=3
# D=3
# p1=2
# p2=0
# q1=1
# q2=0
# P1=1
# P2=0
# Q1=1
# Q2=0

# > model_summary
# ME      RMSE        MAE       MAPE df         AIC       BIC
# ARIMA(2,3,0)(1,3,0)[12] -0.002560354 0.1682577 0.11539638 0.11480231  4  24.0304036  31.51521
# ARIMA(2,3,0)(1,3,1)[12] -0.001350654 0.1360399 0.09525982 0.09488620  5  18.3429149  27.69892
# ARIMA(2,3,0)(0,3,0)[12] -0.003763122 0.2831688 0.19660082 0.19440249  3  60.9227169  66.53632
# ARIMA(2,3,0)(0,3,1)[12] -0.002069462 0.1712971 0.12195953 0.12123320  4  31.4330117  38.91782
# ARIMA(2,3,1)(1,2,0)[12]  0.020740024 0.1037148 0.07600660 0.07660884  5 -54.8389759 -44.36725
# ARIMA(2,3,1)(1,3,1)[12] -0.021317161 0.1214412 0.08327497 0.08314457  6   0.8151851  12.04239
# ARIMA(2,3,1)(0,3,0)[12] -0.019237668 0.2117555 0.14723913 0.14588228  4  35.8255364  43.31034
# ARIMA(2,3,1)(0,3,1)[12] -0.021827044 0.1385238 0.09679543 0.09648321  5  11.1841191  20.54012
# ARIMA(0,3,0)(1,3,0)[12]  0.001606623 0.2886365 0.19147356 0.18936464  2  72.1524884  75.89489
# ARIMA(0,3,0)(1,3,1)[12]  0.005012139 0.1999744 0.13522682 0.13408482  3  59.3099119  64.92351
# ARIMA(0,3,0)(0,3,0)[12]  0.001648472 0.4545665 0.31217868 0.30699711  1 103.4278416 105.29904
# ARIMA(0,3,0)(0,3,1)[12]  0.003555379 0.2751398 0.19154340 0.18926359  2  73.9232582  77.66566
# ARIMA(0,3,1)(1,3,0)[12] -0.017914930 0.1700572 0.11536738 0.11464340  3  22.1560369  27.76964
# ARIMA(0,3,1)(1,3,1)[12] -0.015496053 0.1320286 0.09231744 0.09192552  4  14.3482620  21.83307
# ARIMA(0,3,1)(0,3,0)[12] -0.012592816 0.2618091 0.18447651 0.18209434  2  51.5654074  55.30781
# ARIMA(0,3,1)(0,3,1)[12] -0.013478162 0.1616985 0.11547210 0.11470546  3  25.0335114  30.64711