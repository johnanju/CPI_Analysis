qqnorm(newspaper2) 
qqline(newspaper2)

boxplot(newspaper2~cycle(newspaper2));plot(aggregate.ts(newspaper2))
par(mfrow = c(1,2))
acf(newspaper2);pacf(newspaper2)


plot(aggregate(newspaper2))

#Stationary Food and Beverages

components.newspaper2=decompose(newspaper2)
news2adjusted = newspaper2-components.newspaper2$seasonal
news2stationary= diff(news2adjusted,differences=2)
adf.test(news2stationary,alternative="stationary",k=12) #p<0.05 reject null hypothesis of non stationarity ie stationary.
kpss.test(news2stationary) #accept null hypothesis ie

par(mfrow = c(3,2))
acf(newspaper2)
pacf(newspaper2)
acf(news2adjusted)
pacf(news2adjusted)
acf(newspaper2stationary); 
pacf(newspaper2stationary)

#fsadjusted
#recadjusted
#resadjusted
#newsseasonal
#gadjusted
#fs2adjusted
#rec2adjusted
#res2adjusted
#news2adjusted

x=news2adjusted
#auto=auto.arima(x,stepwise = FALSE, trace = TRUE) 

#fsadjusted   :  Best model: ARIMA(3,1,2) 
#newsseasonal :  Best model: ARIMA(2,2,0)(0,0,1)[12] 
#recadjusted  :  Best model: ARIMA(1,2,0)(1,0,1)[12]  
#resadjusted  :  Best model: ARIMA(1,2,2)
#gadjusted    :  Best model: ARIMA(1,1,2)(1,0,0)[12] with drift  
#fs2adjusted  :  Best model: ARIMA(1,2,0)(2,0,0)[12]  
#rec2adjusted :  Best model: ARIMA(4,2,0)(0,0,1)[12]  
#res2adjusted :  Best model: ARIMA(0,2,2)(1,0,1)[12] 
#news2adjusted:  Best model: ARIMA(0,2,1)


d=3
D=2
p1=1
p2=0
q1=1
q2=0
P1=1
P2=0
Q1=1
Q2=0


fit1 <- arima(x,c(p1,d,q2),seasonal=list(order=c(P1,D,Q2),period=12))
fit2 <- arima(x,c(p1,d,q2),seasonal=list(order=c(P1,D,Q1),period=12))
fit3 <- arima(x,c(p1,d,q2),seasonal=list(order=c(P2,D,Q2),period=12))
fit4 <- arima(x,c(p1,d,q2),seasonal=list(order=c(P2,D,Q1),period=12))

fit5 <- arima(x,c(p1,d,q1),seasonal=list(order=c(P1,D,Q2),period=12))
fit6 <- arima(x,c(p1,d,q1),seasonal=list(order=c(P1,D,Q1),period=12))
fit7 <- arima(x,c(p1,d,q1),seasonal=list(order=c(P2,D,Q2),period=12))
fit8 <- arima(x,c(p1,d,q1),seasonal=list(order=c(P2,D,Q1),period=12))

fit9 <- arima(x,c(p2,d,q2),seasonal=list(order=c(P1,D,Q2),period=12))
fit10 <- arima(x,c(p2,d,q2),seasonal=list(order=c(P1,D,Q1),period=12))
fit11 <- arima(x,c(p2,d,q2),seasonal=list(order=c(P2,D,Q2),period=12))
fit12 <- arima(x,c(p2,d,q2),seasonal=list(order=c(P2,D,Q1),period=12))

fit13 <- arima(x,c(p2,d,q1),seasonal=list(order=c(P1,D,Q2),period=12))
fit14 <- arima(x,c(p2,d,q1),seasonal=list(order=c(P1,D,Q1),period=12))
fit15 <- arima(x,c(p2,d,q1),seasonal=list(order=c(P2,D,Q2),period=12))
fit16 <- arima(x,c(p2,d,q1),seasonal=list(order=c(P2,D,Q1),period=12))

s1= summary(fit1)
s2=	summary(fit2)
s3=	summary(fit3)
s4=	summary(fit4)
s5=	summary(fit5)
s6=	summary(fit6)
s7=	summary(fit7)
s8=	summary(fit8)
s9=	summary(fit9)
s10= summary(fit10)
s11	=summary(fit11)
s12	=summary(fit12)
s13	=summary(fit13)
s14=	summary(fit14)
s15	=summary(fit15)
s16	=summary(fit16)

f1 = forecast(fit1,h=30)
f2 = forecast(fit2,h=30)
f3 = forecast(fit3,h=30)
f4 = forecast(fit4,h=30)
f5 = forecast(fit5,h=30)
f6 = forecast(fit6,h=30)
f7 = forecast(fit7,h=30)
f8 = forecast(fit8,h=30)
f9 = forecast(fit9,h=30)
f10 = forecast(fit10,h=30)
f11 = forecast(fit11,h=30)
f12 = forecast(fit12,h=30)
f13 = forecast(fit13,h=30)
f14 = forecast(fit14,h=30)
f15 = forecast(fit15,h=30)
f16 = forecast(fit16,h=30)


aic=AIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10,fit11,fit12,fit13,fit14,fit15,fit16)
bic=BIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10,fit11,fit12,fit13,fit14,fit15,fit16)

#----Model Summary & comparisom
model_summary = data.frame(ME = c(s1[1],s2[1],s3[1],s4[1],s5[1],s6[1],s7[1],s8[1],s9[1],s10[1],s11[1],s12[1],s13[1],s14[1],s15[1],s16[1]),
                           RMSE =c(s1[2],s2[2],s3[2],s4[2],s5[2],s6[2],s7[2],s8[2],s9[2],s10[2],s11[2],s12[2],s13[2],s14[2],s15[2],s16[2]),
                           MAE=c(s1[3],s2[3],s3[3],s4[3],s5[3],s6[3],s7[3],s8[3],s9[3],s10[3],s11[3],s12[3],s13[3],s14[3],s15[3],s16[3]),
                           MAPE=c(s1[5],s2[5],s3[5],s4[5],s5[5],s6[5],s7[5],s8[5],s9[5],s10[5],s11[5],s12[5],s13[5],s14[5],s15[5],s16[5]),
                           df=c(aic$df),
                           AIC=c(aic$AIC),
                           BIC=c(bic$BIC)
)
rownames(model_summary)=c(f1$method,f2$method,f3$method,f4$method,f5$method,f6$method,f7$method,f8$method,f9$method,f10$method,f11$method,f12$method,f13$method,f14$method,f15$method,f16$method)
model_summary




# > model_summary
#                               ME      RMSE        MAE       MAPE df         AIC        BIC
# ARIMA(1,3,0)(1,2,0)[12] 0.0062918876 0.1887262 0.13046921 0.13168594  3  10.7815830  17.064617
# ARIMA(1,3,0)(1,2,1)[12] 0.0110940327 0.1427414 0.10169193 0.10280920  4  -2.4032092   5.974169
# ARIMA(1,3,0)(0,2,0)[12] 0.0023291873 0.2477973 0.16474778 0.16578363  2  35.4465474  39.635237
# ARIMA(1,3,0)(0,2,1)[12] 0.0090600504 0.1582099 0.11437591 0.11551353  3   2.4689879   8.752022
# ARIMA(1,3,1)(1,2,0)[12] 0.0274546710 0.1380872 0.09363975 0.09470609  4 -22.7670523 -14.389674
# ARIMA(1,3,1)(1,2,1)[12] 0.0285234804 0.1170406 0.08315865 0.08419736  5 -30.4554219 -19.983699
# ARIMA(1,3,1)(0,2,0)[12] 0.0238618311 0.1775419 0.12190629 0.12286078  3  -0.2647232   6.018310
# ARIMA(1,3,1)(0,2,1)[12] 0.0272490434 0.1241592 0.08824272 0.08926341  4 -27.0849853 -18.707607
# ARIMA(0,3,0)(1,2,0)[12] 0.0034924586 0.2461531 0.17074341 0.17238853  2  39.0645263  43.253215
# ARIMA(0,3,0)(1,2,1)[12] 0.0053662389 0.1822113 0.12684115 0.12829959  3  25.4429558  31.725989
# ARIMA(0,3,0)(0,2,0)[12] 0.0001656132 0.2977619 0.20096745 0.20253869  1  55.9142349  58.008579
# ARIMA(0,3,0)(0,2,1)[12] 0.0044565658 0.1942777 0.13448406 0.13600962  2  26.5078741  30.696563
# ARIMA(0,3,1)(1,2,0)[12] 0.0275192205 0.1526431 0.10548126 0.10670989  3 -14.0162905  -7.733257
# ARIMA(0,3,1)(1,2,1)[12] 0.0282146136 0.1252226 0.08841028 0.08958097  4 -23.0645439 -14.687166
# ARIMA(0,3,1)(0,2,0)[12] 0.0232434484 0.1884737 0.13187501 0.13294367  2   4.4482325   8.636922
# ARIMA(0,3,1)(0,2,1)[12] 0.0268978706 0.1309405 0.09415987 0.09530955  3 -21.6621663 -15.379133
# > 