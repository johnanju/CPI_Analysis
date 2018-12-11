
setwd("C:/Documents/Thesis/Thesis")

data = loadWorkbook("CPM15.xlsx")
df <- rename(data,c('Consumer.Price.Index.by.Month..Detailed.Sub.Indices.and.Statistic'='Year','NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
df <- df[c(-1,-2),]
df <- df[(-190:-210),]
head(df)
tail(df)
View(df)
df$Year=lubridate::ymd(df$Year,truncated=2L)
df1 <- df
str(df)

df$`Food & Beverages` <- as.numeric(as.character(df$`Food & Beverages`))
df$`Recreation & Culture` <- as.numeric(as.character(df$`Recreation & Culture`))
df$`Newspaper & Publications` <- as.numeric(as.character(df$`Newspaper & Publications`))
df$`Restaurants & Accomodation` <- as.numeric(as.character(df$`Restaurants & Accomodation`))
df$`Goods & Services` <- as.numeric(as.character(df$`Goods & Services`))



str(df)

str(df1)

View(df)
View(df1)
# Tue Nov 27 01:33:29 2018 ------------------------------

tsData = data.matrix(df)
tsData = ts(tsData ,start = c(2003,1),end = c(2018,9),frequency = 12)
View(tsData)
class(tsData)
tail(tsData)
plot(tsData[,-1])
class(tsData)
autoplot(tsData[,-1]) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

scatterplotMatrix(tsData[,-1])
sapply(tsData[,-1],mean)
sapply(tsData[,-1],sd)

#decompose

deFood = decompose(tsData[,2])
deRecreation = decompose(tsData[,3])
deNews = decompose(tsData[,4])
deRestaurant = decompose(tsData[,5])
deGoods = decompose(tsData[,6])
plot(deFood)
plot(deRecreation)
plot(deNews)
plot(deGoods)

#Before
tsBefore2011 = window(tsData[,-1],start=c(2003,1),end=c(2011,6))
plot(tsBefore2011)

tsAfter2011=window(tsData[,-1],start=c(2011,7),end=c(2018,9))
plot(tsAfter2011)



fit1 = stl(food1,"per")
plot(fit1)
fit2 =stl(recreation1,"per")
plot(fit2)
fit3 =stl(newspaper1,"per")
plot(fit3)
fit4 =stl(restaurants1,"per")
plot(fit4)
fit5 =stl(goods1,"per")
plot(fit5)
#Step 4: Stationarity:Augmented Dickey-Fuller Test

adf.test(food1,alternative = "stationary")
adf.test(recreation1,alternative = "stationary")
adf.test(newspaper1,alternative = "stationary")
adf.test(restaurants1,alternative = "stationary")
adf.test(goods1,alternative = "stationary")

#Step 5: Autocorrelations and Choosing Model Order
acf(food1,main='')
pacf(food1,main='')

acf(diff(log(food1)))
pacf(diff(log(food1)))
WWWusage %>%
  model1 = arima(window(log(food1),end=c(2011,6)),c(0,1,1),seasonal=list(orders=c(0,1,1),period=12)) %>%
  forecas=forecast(model1,30)%>%
  autoplot
plot(forecast(model1,30))
autoplot
lines(food1)

model2=arima(window(food1,start=c(2003),model=model1))
accuracy(model1)
accuracy(model2)
pred=predict(fit,n.head=12)
ts.plot(food1,2.718^pred$pred,log="y",lty=c(1,3))




fit = auto.arima(food1)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
fit2 =arima(food1,order = c(2,1,0),seasonal = list(order=c(0,0,0)))

fcast = forecast(fit2,h=20)
autoplot
plot(fcast)
bfcast_no_holdout= forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout)
lines(tsData[,-1])


library("fUnitRoots")
urkpssTest(food1, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(food1,differences=1)
plot(tsstationary)

#After


df = data.frame(date = index(amzn.tkr), amzn.tkr, row.names=NULL)

ggplot(df, aes(df$Year, df$`Food & Beverages`)) + geom_line() + scale_x_date('Month/2017')  + ylab("AMZN Adjusted Stock Price") +
  xlab("") + labs(title = "Amazone Stock Price")







#---------------------Food&NonAlcoholic Beverages-------------
food1= window(tsBefore2011[,1],start=c(2003,1),end=c(2011,6))
fit1 = stl(food1,"per")
plot(fit1)

#----stationarity test 1-----
library("fUnitRoots")
urkpssTest(food1,type = c("tau"), lags = c("short"))
kpss.test(food1, null = "Trend")


#seasonality
tsstationary = diff(food1, differences=1)
plot(tsstationary)

timeseriesseasonallyadjusted <- tsData- components.tsData$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)
plot(tsstationary)
str(tsstationary)
acf(tsstationary)
pacf(tsstationary)

tsstationary <- diff(timeseriesseasonallyadjusted, differences=2)
plot(tsstationary)
acf(tsstationary)

timeseriesseasonallyadjusted <- tsData- components.tsData$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=3)
plot(tsstationary)
acf(tsstationary)

#Step 4: Stationarity:Augmented Dickey-Fuller Test

adf.test(food1,alternative = "stationary",k=12)
adf.test(tsstationary,alternative="stationary",k=12)
adf.test(recreation1,alternative = "stationary")
adf.test(newspaper1,alternative = "stationary")
adf.test(restaurants1,alternative = "stationary")
adf.test(goods1,alternative = "stationary")

#Step 5: Autocorrelations and Choosing Model Order
acf(food1,main='')
pacf(food1,main='')

#Auto ARIMA
model_1=auto.arima(food1,stepwise = FALSE, trace = TRUE)
summary(model_1)
str(model_1)
coeftest(model_1)
confint(foodModel1)

model_3 <- arima(food1, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12))

summary(model_3)
coeftest(model_3)

model_2 <- Arima(food1, order = c(1,1,0), seasonal = list(order = c(0,0,1), period = 12))
summary(model_2)
coeftest(model_2)

model_4 <- Arima(food1, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 12))
summary(model_4)
coeftest(model_4)



#-----intervention model ------
ao=filter(tsData, filter = 0, method = "recursive")
plot(ao, main = "Additive Outlier - TC delta = 0", type = "s")
tc = window(tsData,start=c(2005,1))
tc_0_4 <- filter(tc, filter = 0.4, method = "recursive")
tc_0_8 <- filter(tc, filter = 0.8, method = "recursive")
plot(tc_0_4, main = "TC delta = 0.4")
plot(tc_0_8, main = "TC delta = 0.8")



x <- data.frame("AIC" = 1:2, "Age" = c(21,15), "Name" = c("John","Dora"))

aic = c(model_1$aic, model_2$aic, model_3$aic)
aicc <-c(model_1$aicc, model_2$aicc, model_3$aicc)
bic <-c(model_1$bic, model_2$bic, model_3$bic)


modelComp <- data.frame(c = c(model_1$aic, model_2$aic, model_4$aic),
                        col_2_res = c(model_1$aicc, model_2$aicc, model_3$aicc, model_4$aicc),
                        col_3_res = c(model_1$bic, model_2$bic, model_3$bic, model_4$bic))

colnames(modelComp) <- c("AIC", "AICc", "BIC")
rownames(modelComp) <- c("ARIMA(1,1,1)", 
                         "ARIMA(1,0,0)(0,0,1)[10]", 
                         "ARIMA(1,0,0)(1,0,0)[10]")
modelComp





