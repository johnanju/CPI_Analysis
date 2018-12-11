fa1=fsadjusted
ra1=recadjusted
rs1=resadjusted
n1=newsseasonal
g1=gadjusted
fa2=fs2adjusted
ra2=rec2adjusted
rs2=res2adjusted
n2=news2adjusted
g2=g2adjusted

fa1auto=auto.arima(fa1,d=1,stepwise = FALSE, trace = TRUE) 
ra1auto=auto.arima(ra1,d=2,stepwise = FALSE, trace = TRUE) 
rs1auto=auto.arima(rs1,d=2,stepwise = FALSE, trace = TRUE) 
n1auto=auto.arima(n1,d=2,stepwise = FALSE, trace = TRUE) 
g1auto=auto.arima(g1,d=1,stepwise = FALSE, trace = TRUE) 
fa2auto=auto.arima(fa2,d=2,stepwise = FALSE, trace = TRUE) 
ra2auto=auto.arima(ra2,d=2,stepwise = FALSE, trace = TRUE) 
rs2auto=auto.arima(rs2,d=2,stepwise = FALSE, trace = TRUE) 
n2auto=auto.arima(n2,d=2,stepwise = FALSE, trace = TRUE) 
g2auto=auto.arima(g2,d=0,stepwise = FALSE, trace = TRUE) 

#fsadjusted   :  Best model: ARIMA(3,1,2) 
#newsseasonal :  Best model: ARIMA(2,2,0)(0,0,1)[12] 
#recadjusted  :  Best model: ARIMA(1,2,0)(1,0,1)[12]  
#resadjusted  :  Best model: ARIMA(1,2,2)
#gadjusted    :  Best model: ARIMA(1,1,2)(1,0,0)[12] with drift  
#fs2adjusted  :  Best model: ARIMA(1,2,0)(2,0,0)[12]  
#rec2adjusted :  Best model: ARIMA(4,2,0)(0,0,1)[12]  
#res2adjusted :  Best model: ARIMA(0,2,2)(1,0,1)[12] 
#news2adjusted:  Best model: ARIMA(0,2,1)
#g2adjusted   :  Best model: ARIMA(3,0,0)(0,0,1)[12] with non-zero mean 


#seasonal difference =0
a1=auto.arima(fa1,d=1,stepwise = FALSE, trace = TRUE) 
b1=auto.arima(ra1,d=2,stepwise = FALSE, trace = TRUE) 
c1=auto.arima(rs1,d=2,stepwise = FALSE, trace = TRUE) 
d1=auto.arima(n1,d=2,stepwise = FALSE, trace = TRUE) 
e1=auto.arima(g1,d=1,stepwise = FALSE, trace = TRUE) 
j1=auto.arima(fa2,d=2,stepwise = FALSE, trace = TRUE) 
l1=auto.arima(ra2,d=2,stepwise = FALSE, trace = TRUE) 
h1=auto.arima(rs2,d=2,stepwise = FALSE, trace = TRUE) 
i1=auto.arima(n2,d=2,stepwise = FALSE, trace = TRUE) 
k1=auto.arima(g2,d=0,stepwise = FALSE, trace = TRUE) 
#seasonal difference =1
a2=auto.arima(fa1,d=1,D=1,stepwise = FALSE, trace = TRUE) 
b2=auto.arima(ra1,d=2,D=1,stepwise = FALSE, trace = TRUE) 
c2=auto.arima(rs1,d=2,D=1,stepwise = FALSE, trace = TRUE) 
d2=auto.arima(n1,d=2,D=1,stepwise = FALSE, trace = TRUE) 
e2=auto.arima(g1,d=1,D=1,stepwise = FALSE, trace = TRUE) 
j2=auto.arima(fa2,d=2,D=1,stepwise = FALSE, trace = TRUE) 
l2=auto.arima(ra2,d=2,D=1,stepwise = FALSE, trace = TRUE) 
h2=auto.arima(rs2,d=2,D=1,stepwise = FALSE, trace = TRUE) 
i2=auto.arima(n2,d=2,D=1,stepwise = FALSE, trace = TRUE) 
k2=auto.arima(g2,d=0,D=1,stepwise = FALSE, trace = TRUE)
#seasonal difference =2
a3=auto.arima(fa1,d=1,D=2,stepwise = FALSE, trace = TRUE) 
b3=auto.arima(ra1,d=2,D=2,stepwise = FALSE, trace = TRUE) 
c3=auto.arima(rs1,d=2,D=2,stepwise = FALSE, trace = TRUE) 
d3=auto.arima(n1,d=2,D=2,stepwise = FALSE, trace = TRUE) 
e3=auto.arima(g1,d=1,D=2,stepwise = FALSE, trace = TRUE) 
j3=auto.arima(fa2,d=2,D=2,stepwise = FALSE, trace = TRUE) 
l3=auto.arima(ra2,d=2,D=2,stepwise = FALSE, trace = TRUE) 
h3=auto.arima(rs2,d=2,D=2,stepwise = FALSE, trace = TRUE) 
i3=auto.arima(n2,d=2,D=2,stepwise = FALSE, trace = TRUE) 
k3=auto.arima(g2,d=0,D=2,stepwise = FALSE, trace = TRUE)


a_summary=data.frame(a1=accuracy(a1),
              a2=accuracy(a2),a3=accuracy(a3))
b_summary=data.frame(b1=accuracy(b1),
                    b2=accuracy(b2),b3=accuracy(b3))

aic=AIC(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3,e1,e2,e3,j1,j2,j3,l1,l2,l3,h1,h2,h3,i1,i2,i3,k1,k2,k3)                                    
aic
bic=BIC(a1,a2,a3,b1,b2,b3,c1,c2,c3,d1,d2,d3,e1,e2,e3,j1,j2,j3,l1,l2,l3,h1,h2,h3,i1,i2,i3,k1,k2,k3)
bic


checkresiduals(a1)# best model
checkresiduals(a2)
checkresiduals(a3)
checkresiduals(b1)
checkresiduals(b2)
checkresiduals(b3)

s1= summary(a1)
s2=	summary(a2)
s3=	summary(a3)
s4=	summary(b1)
s5=	summary(b2)
s6=	summary(b3)
s7=	summary(c1)
s8=	summary(c2)
s9=	summary(c3)
s10= summary(d1)
s11	=summary(d2)
s12	=summary(d3)
s13	=summary(e1)
s14=	summary(e2)
s15	=summary(e3)
s16	=summary(j1)
s17=summary(j2)
s18=summary(j3)
s19=summary(l1)
s20=summary(l2)
s21=summary(l3)
s22=summary(h1)
s23=summary(h2)
s24=summary(h3)
s25=summary(i1)
s26=summary(i2)
s27=summary(i3)
s28=summary(k1)
s29=summary(k2)
s30=summary(k3)

f1 = forecast(a1,h=30)
f2 = forecast(a2,h=30)
f3 = forecast(a3,h=30)
f4 = forecast(b1,h=30)
f5 = forecast(b2,h=30)
f6 = forecast(b3,h=30)
f7 = forecast(c1,h=30)
f8 = forecast(c2,h=30)
f9 = forecast(c3,h=30)
f10 = forecast(d1,h=30)
f11 = forecast(d2,h=30)
f12 = forecast(d3,h=30)
f13 = forecast(e1,h=30)
f14 = forecast(e2,h=30)
f15 = forecast(e3,h=30)
f16 = forecast(j1,h=30)
f17 = forecast(j2,h=30)
f18 = forecast(j3,h=30)
f19= forecast(l1,h=30)
f20= forecast(l2,h=30)
f21= forecast(l3,h=30)
f22= forecast(h1,h=30)
f23= forecast(h2,h=30)
f24= forecast(h3,h=30)
f25 = forecast(i1,h=30)
f26 = forecast(i2,h=30)
f27 = forecast(i3,h=30)
f28 = forecast(k1,h=30)
f29 = forecast(k2,h=30)
f30 = forecast(k3,h=30)



aic=AIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10,fit11,fit12,fit13,fit14,fit15,fit16)
bic=BIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10,fit11,fit12,fit13,fit14,fit15,fit16)

#----Model Summary & comparisom
model_summary = data.frame(ME = c(s1[1],s2[1],s3[1],s4[1],s5[1],s6[1],s7[1],s8[1],s9[1],s10[1],s11[1],s12[1],s13[1],s14[1],s15[1],s16[1],s17[1],s18[1],s19[1],s20[1],s21[1],s22[1],s23[1],s24[1],s25[1],s26[1],s27[1],s28[1],s29[1],s30[1]),
                           RMSE =c(s1[2],s2[2],s3[2],s4[2],s5[2],s6[2],s7[2],s8[2],s9[2],s10[2],s11[2],s12[2],s13[2],s14[2],s15[2],s16[2],s17[2],s18[2],s19[2],s20[2],s21[2],s22[2],s23[2],s24[2],s25[2],s26[2],s27[2],s28[2],s29[2],s30[2]),
                           MAE=c(s1[3],s2[3],s3[3],s4[3],s5[3],s6[3],s7[3],s8[3],s9[3],s10[3],s11[3],s12[3],s13[3],s14[3],s15[3],s16[3],s17[3],s18[3],s19[3],s20[3],s21[3],s22[3],s23[3],s24[3],s25[3],s26[3],s27[3],s28[3],s29[3],s30[3]),
                           MAPE=c(s1[5],s2[5],s3[5],s4[5],s5[5],s6[5],s7[5],s8[5],s9[5],s10[5],s11[5],s12[5],s13[5],s14[5],s15[5],s16[5],s17[5],s18[5],s19[5],s20[5],s21[5],s22[5],s23[5],s24[5],s25[5],s26[5],s27[5],s28[5],s29[5],s30[5]),
                           df=c(aic$df),
                           AIC=c(aic$AIC),
                           BIC=c(bic$BIC)
)
rownames(model_summary)=c(f1$method,f2$method,f3$method,f4$method,f5$method,f6$method,f7$method,f8$method,f9$method,f10$method,
                          f11$method,f12$method,f13$method,f14$method,f15$method,f16$method,f17$method,f18$method,f19$method,f20$method,
                          f21$method,f22$method,f23$method,f24$method,f25$method,f26$method,f27$method,f28$method,f29$method,f30$method)
model_summary
#---------------------------------------
ffood1=data.frame(a1=c(accuracy(a1),aic$df[1],aic$AIC[1],bic$BIC[1]),
                  a2=c(accuracy(a2),aic$df[2],aic$AIC[2],bic$BIC[2]),
                  a3=c(accuracy(a3),aic$df[3],aic$AIC[3],bic$BIC[3]))
rownames(ffood1)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(ffood1) = c(f1$method,f2$method,f3$method)
ffood1

checkresiduals(a1)# best model
checkresiduals(a2)
checkresiduals(a3)

#---------------------------------------

frecreation1 =data.frame(b1=c(accuracy(b1),aic$df[4],aic$AIC[4],bic$BIC[4]),
                          b2=c(accuracy(b2),aic$df[5],aic$AIC[5],bic$BIC[5]),
                          b3=c(accuracy(b3),aic$df[6],aic$AIC[6],bic$BIC[6]))
rownames(frecreation1)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(frecreation1) = c(f4$method,f5$method,f6$method) 
frecreation1

checkresiduals(b1) # ACF is significant at Lag21
checkresiduals(b2) # best model
checkresiduals(b3)

#---------------------------------------

frestaurants1=data.frame(c1=c(accuracy(c1),aic$df[7],aic$AIC[7],bic$BIC[7]),
                        c2=c(accuracy(c2),aic$df[8],aic$AIC[8],bic$BIC[8]),
                        c3=c(accuracy(c3),aic$df[9],aic$AIC[9],bic$BIC[9]))
rownames(frestaurants1)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(frestaurants1) = c(f7$method,f8$method,f9$method) 
frestaurants1
checkresiduals(c1)#best model
checkresiduals(c2)
checkresiduals(c3)
#---------------------------------------

fnewspaper1=data.frame(d1=c(accuracy(d1),aic$df[10],aic$AIC[10],bic$BIC[10]),
                       d2=c(accuracy(d2),aic$df[11],aic$AIC[11],bic$BIC[11]),
                       d3=c(accuracy(d3),aic$df[12],aic$AIC[12],bic$BIC[12]))
rownames(fnewspaper1)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(fnewspaper1) = c(f10$method,f11$method,f12$method) 
fnewspaper1
checkresiduals(d1)# best model
checkresiduals(d2)
checkresiduals(d3)
#---------------------------------------
#needs re visitng 
fgoods1 =   data.frame(e1=c(accuracy(e1),aic$df[13],aic$AIC[13],bic$BIC[13]),
                       e2=c(accuracy(e2),aic$df[14],aic$AIC[14],bic$BIC[14]),
                       e3=c(accuracy(e3),aic$df[15],aic$AIC[15],bic$BIC[15]))
rownames(fgoods1)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(fgoods1) = c(f13$method,f14$method,f15$method) 
fgoods1
checkresiduals(e1)#lag 13,14is significant
checkresiduals(e2)#lag 13,14 significant 
checkresiduals(e3)#best model
f
#---------------------------------------
ffood2 =    data.frame(j1=c(accuracy(j1),aic$df[16],aic$AIC[16],bic$BIC[16]),
                       j2=c(accuracy(j2),aic$df[17],aic$AIC[17],bic$BIC[17]),
                       j3=c(accuracy(j3),aic$df[18],aic$AIC[18],bic$BIC[18]))
rownames(ffood2)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(ffood2) = c(f16$method,f17$method,f18$method) 
ffood2
checkresiduals(j1)#best model
checkresiduals(j2)
checkresiduals(j3)
#---------------------------------------
#need to try different differencing
frecreation2 = data.frame(l1=c(accuracy(l1),aic$df[19],aic$AIC[19],bic$BIC[19]),
                           l2=c(accuracy(l2),aic$df[20],aic$AIC[20],bic$BIC[20]),
                           l3=c(accuracy(l3),aic$df[21],aic$AIC[21],bic$BIC[21]))
rownames(frecreation2)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(frecreation2) = c(f19$method,f20$method,f21$method) 
frecreation2
checkresiduals(l1)#lag 13 ,23 significant
checkresiduals(l2)
checkresiduals(l3)
#---------------------------------------

frestaurants2 = data.frame(h1=c(accuracy(h1),aic$df[22],aic$AIC[22],bic$BIC[22]),
                          h2=c(accuracy(h2),aic$df[23],aic$AIC[23],bic$BIC[23]),
                          h3=c(accuracy(h3),aic$df[24],aic$AIC[24],bic$BIC[24]))
rownames(frestaurants2)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(frestaurants2) = c(f22$method,f23$method,f24$method) 
frestaurants2
checkresiduals(h1)#best model
checkresiduals(h2)# lag 1 sugnificant , overdifferenced
checkresiduals(h3)#lag 15 significant
#---------------------------------------

fnewspaper2 = data.frame(i1=c(accuracy(i1),aic$df[25],aic$AIC[25],bic$BIC[25]),
                           i2=c(accuracy(i2),aic$df[26],aic$AIC[26],bic$BIC[26]),
                           i3=c(accuracy(i3),aic$df[27],aic$AIC[27],bic$BIC[27]))
rownames(fnewspaper2)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(fnewspaper2) = c(f25$method,f26$method,f27$method) 
fnewspaper2
checkresiduals(i1)
checkresiduals(i2)# lag 11 sugnificant , overdifferenced
checkresiduals(i3)#best model
#----------------------------------------------------------
fgoods2 =     data.frame(k1=c(accuracy(k1),aic$df[28],aic$AIC[28],bic$BIC[28]),
                         k2=c(accuracy(k2),aic$df[29],aic$AIC[29],bic$BIC[29]),
                         k3=c(accuracy(k3),aic$df[30],aic$AIC[30],bic$BIC[30]))
rownames(fgoods2)=c('ME','RMSE','MAE','MPE','MAPE','MASE','ACF1','df','AIC','BIC')
colnames(fgoods2) = c(f28$method,f29$method,f30$method) 
fgoods2
checkresiduals(k1) #best model
checkresiduals(k2)# lag 11 sugnificant , overdifferenced
checkresiduals(k3)
#-----------------------------------------------------------

t.test(f1$fitted,f16$fitted,conf.level = 0.90)
t.test(f5$fitted,f20$fitted,conf.level = 0.90)
t.test(f7$fitted,f22$fitted,conf.level = 0.90)
t.test(f10$fitted,f27$fitted,conf.level = 0.90)
t.test(f15$fitted,f28$fitted,conf.level = 0.90)

t.test(f1$fitted,f16$x)
t.test(f5$fitted,f20$x)
t.test(f7$fitted,f22$x)
t.test(f10$fitted,f27$x)
t.test(f15$fitted,f28$x)

plot(predict(f16))


predict.food1=predict(a1)
anova(a1,j1)
coef.FitARMA(j1)
lmtest::coeftest(a1)
lmtest::jtest(a1,j1)



