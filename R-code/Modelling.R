#---------------------------------------------------------------------------
install.packages("xlsx")
install.packages("lubridate")
install.packages("plyr")
install.packages("fpp2")
install.packages("fUnitRoots")
install.packages("ggplot2")
install.packages("forecast")
install.packages("asta")
install.packages("lmtest")
install.packages("FitARMA")
install.packages("strucchange")
install.packages("reshape")
install.packages("Rmisc")
install.packages("fBasics")
install.packages("tSeries")
install.packages("urca")
install.packages("xts")
install.packages("pls")
install.packages("caret")

library("urca")
library("tseries")
library("xlsx")
library("rJava")
library("fpp2")
library("plyr")
library('ggplot2')
library('forecast')
library('tseries')
library("car")
library("asta")
library("lmtest")
library("FitARMA")
library("strucchange")
library("reshape")
library("Rmisc")
library("fBasics")
library("fUnitRoots")
library("xts")
library("pls")
library("caret")


suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(astsa))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(fUnitRoots))
suppressPackageStartupMessages(library(FitARMA))
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(Rmisc))
suppressPackageStartupMessages(library(fBasics))



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


