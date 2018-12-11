
data = read.xlsx("CPM16_5.xlsx",1)
columnname(data)[1]= "year"
colnames(data)[1]="Year"
install.packages("plyr")
library(plyr)
df <- rename(df,c('NA'='Food & beverages', 'NA..1'='Recreation & Culture'))
df <- rename(df,c('NA.'='Food & beverages', 'NA..1'='Recreation & Culture'))
df <- rename(df,c('NA.'='Food & beverages', 'NA..1'='Recreation & Culture'))
df <- rename(data,c('NA.'='Food & beverages', 'NA..1'='Recreation & Culture'))
head(df)
df <- rename(data,c('NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
head(df)
df <- df(c[-1,-2,])
df <- df(c[(-1,-2),])
df <- df(c[-1,])
df <- df[(-1,-2),]
df <- df[-1,]
head(df)
df <- df[-2,]
df
df <- df[-1,]
head(df)
str(df$Year)
install.packages("zoo")
library("zoo")
as.Date(df$Year,%Y/%m)
as.Date(df$Year,"%Y-%m")
data = read.xlsx("CPM16_5.xlsx",1)
colnames(data)[1]="Year"
names = [Year,F&non alcholoic, REcreation and culture, newspaper, restaurants,misc]
library(plyr)
df <- rename(data,c('NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
head(df)
df <- df[-1,]
df <- df[-2,]
df
df <- df[-1,]
head(df)
yrs <- c(2001, 2002, 2002, 2002, 2003, 2005)
lubridate::ymd(yrs, truncated = 2L)
lubridate::ym(df$Year,truncated=2L)
lubridate::ymd(df$Year,truncated=2L)
View(df)
head(df)
str(df)
str(yrs)
df$Year=lubridate::ymd(df$Year,truncated=2L)
head(df)
df$Year = as.yearmon("%Y-%m")
head(df)
data = read.xlsx("CPM16_5.xlsx",1)
colnames(data)[1]="Year"
names = [Year,F&non alcholoic, REcreation and culture, newspaper, restaurants,misc]
library(plyr)
df <- rename(data,c('NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
head(df)
df <- df[-1,]
df <- df[-2,]
df
df <- df[-1,]
head(df)
df$Year = as.yearmon("%Y-%m")
head(df)
data = read.xlsx("CPM16_5.xlsx",1)
colnames(data)[1]="Year"
df <- rename(data,c('NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
df$Year=lubridate::ymd(df$Year,truncated=2L)
str(df)
df$Year = as.date.yearmon("%Y-%m")
head(df)
df <- df[-1,]
df <- df[-2,]
df
str(df)
str(yrs)
head(df)
df$Year=as.Date.yearmon("%Y-%m")
df$Year=as.Date("%Y-%m")
head(df)
View(df)
df <- df[c(-192:210)]
df <- df[c(-192:-210)]
df
View(df)
df <- df[r(-192:-210)]
df <- df[(-192:-210),]
df <- df[-2,]
df
df <- df[-1,]
head(df)
install.packages("ffp2")
library("ffp2")
install.packages("timeSeries")
as.ts(df,start=2003,end=2018,frequency=12)
as.ts(df$Year,start=2003,end=2018,frequency=12)
str(df)
df=as.vector(df)
str(df)
library(lubridate)
df = as.vector(read.xlsx("LRA03.xlsx",sheetIndex =1))
str(df)
data = read.xlsx("CPM16_5.xlsx",1)
df <- rename(data,c('NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
df <- df[-1,]
df <- df[-2,]
df <- df[-1,]
df$Year=lubridate::ymd(df$Year,truncated=2L)
str(df)
df$Year=lubridate::ymd(df$Year,truncated=2L)
colnames(data)[1]="Year"
df$Year=lubridate::ymd(df$Year,truncated=2L)
tsData = ts(df, start = c(2003,1), frequency = 12)
View(tsData)
View(tsData)
colnames(data)[1]="Year"
head(df)
colnames(df)[1]="Year"
head(df)
tsData = ts(df$Year, start = c(2003,1), frequency = 12)
tsData = df
tsData$Year = ts(tsData$Year, start = c(2003,1), frequency = 12)
components.ts = decompose(tsData)
data = read.xlsx("CPM16_5.xlsx",1)
colnames(data)[1]="Year"
df <- rename(data,c('NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
head(df)
df <- df[-1,]
df <- df[-1,]
head(df)
df$Year=lubridate::ymd(df$Year,truncated=2L)
df <- df[(-192:-210),]
as.ts(df$Year,start=2003,end=2018,frequency=12)
tsData = df
tsData$Year = ts(tsData$Year, start = c(2003,1), frequency = 12)
components.ts = decompose(tsData)
plot(components.ts)
plot(tsData)
tsData
fit <- stl(tsData, s.window="period")
plot(fit)
tsData$Year = ts(data.matrix(tsData$Year, start = c(2003,1), frequency = 12))
tsData = data.matrix(df)
tsData= ts(matrix(tsData,,5) start = c(2003,1), frequency = 12))
tsData= ts(matrix(tsData,5) start = c(2003,1), frequency = 12))
tsData= ts((tsData,5) start = c(2003,1), frequency = 12))
tsData= ts(matrix(tsData,192,5) start = c(2003,1), frequency = 12))
tsData= ts(matrix(tsData,192,5) start = c(2003,1), frequency = 12)
tsData= ts(matrix(tsData,192,5) ,start = c(2003,1), frequency = 12)
components.ts = decompose(tsData)
plot(tsData)
df <- df[(-192:-210),]
df
tail(df)
df <- df[(-191:-210),]
tail(df)
df <- df[(-190:-210),]
tail(df)
tsData = data.matrix(df)
tsData= ts(matrix(tsData,192,5) ,start = c(2003,1), frequency = 12)
components.ts = decompose(tsData)
tsData= ts(matrix(tsData,190,5) ,start = c(2003,1), frequency = 12)
tsData= ts(matrix(tsData,191,5) ,start = c(2003,1), frequency = 12)
components.ts = decompose(tsData)
plot(tsData)
components.ts = decompose(tsData)
plot(tsData)
components.ts
tsData= ts(matrix(tsData,191,5) ,start = c(2003,1), frequency = 12)
plot(tsData)
plot(tsData)
tsData= ts(matrix(tsData,191,5) ,start = c(2003,1), frequency = 12)
tail(tsData)
plot(tsData)
fit <- stl(tsData, s.window="period")
components.ts = decompose(tsData)
plot(components.ts)
library("fUnitRoots")
install.packages("fUnitRoots")
library("fUnitRoots")
urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsData, differences=1)
plot(tsstationary)
acf(tsData,lag.max=34)
class(tsData)
plot(tsData)
tsData= ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,9) frequency = 12)
tail(tsData)
components.ts = decompose(tsData)
components.ts = decompose(tsData)
plot(components.ts)
class(tsData)
tail(tsData)
plot(components.ts)
plot(tsData)
plot(tsData)
tail(tsData)
library("fUnitRoots")
urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsData, differences=1)
plot(components.ts)
plot(tsData,plot.type ="single",lty=1:3)
plot(tsData,plot.type ="single",lty=1:5)
components.ts = decompose(tsData)
components.ts = decompose(tsData)
plot(components.ts)
plot(components.ts,plot.type ="single",lty=1:5)
plot(components.ts,plot.type ="single",lty=1:3)
plot(components.ts,plot.type ="single",lty=1:3)
decompose(tsData)
View(data)
install.packages("fpp2")
library("fpp2")
utoplot(tsData) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
autoplot(tsData) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
autoplot(tsData$Series2) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
autoplot(tsData) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
tsData1 = tsData[c(,-2)]
tsData1 = tsData[c(-2)]
autoplot(tsData1) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
tsData1= ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,9) frequency = 12)
tsData1= ts(matrix(tsData1,191,5) ,start = c(2003,1),end = c(2018,9) frequency = 12)
tsData1= ts(matrix(tsData1,191,4) ,start = c(2003,1),end = c(2018,9) frequency = 12)
tsData1 = data.matrix(df)[c(-2)]
tsData1= ts(matrix(tsData1,191,4) ,start = c(2003,1),end = c(2018,9) frequency = 12)
tsData1 = data.matrix(df)[c(-2)]
tsData1= ts(matrix(tsData1,191,4) ,start = c(2003,1),end = c(2018,9) frequency = 12)
(df)[c(-2)]
data.matrix(df)[c(-2)]
tsData1= ts(matrix(tsData1,191,4) ,start = c(2003,1),end = c(2018,9) frequency = 12)
tsData1 = data.matrix(df)[c(-2)]
tsData = data.matrix(df)
tsData1 = data.matrix((df)[c(-2)])
tsData1= ts(matrix(tsData1,191,4) ,start = c(2003,1),end = c(2018,9) frequency = 12)
autoplot(tsData1) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
tsData1= ts(matrix(tsData1,191,5) ,start = c(2003,1),end = c(2018,9) frequency = 12)
tsData= ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,9) frequency = 12)
tsData= ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,9) frequency = 12)
tsData= ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,9),frequency = 12)
tail(tsData)
plot(components.ts)
components.ts = decompose(tsData)
plot(components.ts)
plot(tsData)
plot(components.ts,plot.type ="single",lty=1:3)
class(tsData)
autoplot(tsData) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
df <- rename(data,c('NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
data = read.xlsx("CPM16_5.xlsx",1)
data = read.xlsx("CPM16_5.xlsx",1)
head(data)
df <- rename(data,c('Consumer.Price.Index.by.Month..Detailed.Sub.Indices.and.Statistic'='Year','NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
df <- df[(-1,-2),]
df <- df[c(-1,-2),]
df <- df[(-190:-210),]
head(df)
tail(df)
tsData = data.matrix(df)
tsData= ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,9),frequency = 12)
plot(tsData)
tsData = data.matrix(df)
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,1),frequency = 12)
plot(tsData)
components.ts = decompose(tsData)
components.ts = decompose(tsData)
plot(components.ts)
plot(tsData)
View(tsData)
tsData = ts(matrix(tsData,192,5) ,start = c(2003,1),end = c(2018,1),frequency = 12)
tsData = ts(matrix(tsData,182,5) ,start = c(2003,1),end = c(2018,1),frequency = 12)
plot(tsData)
tail(tsData)
tsData = ts(matrix(tsData,182,5) ,start = c(2003,1),end = c(2018,1),frequency = 12)
tsData = ts(matrix(tsData,180,5) ,start = c(2003,1),end = c(2018,1),frequency = 12)
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,1),frequency = 12)
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,1),frequency = 12)
tail(tsData)
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,1),frequency = 12)
tail(tsData)
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,5),frequency = 12)
tail(tsData)
plot(tsData)
tsData = data.matrix(df)
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,5),frequency = 12)
tail(tsData)
plot(tsData)
View(tsData)
tsData = data.matrix(df)
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,5),frequency = 12)
autoplot(tsData) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2020,5),frequency = 12)
tail(tsData)
autoplot(tsData) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
tsData = data.matrix(df)
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2020,5),frequency = 12)
autoplot(tsData) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")
tsData = data.matrix(df)
tsData = ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2020,5),frequency = 12)
