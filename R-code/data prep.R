#-----Data Preparation-------

data = read.xlsx("CPM15_ma.xlsx",1)
head(data)

df <- rename(data,c('Harmonised.Consumer.Price.Index.by.Month..Statistic.and.Commodity.Group'='Year','NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
df <- df[c(-1,-2),]
df <- df[(-190:-210),]
head(df)

df$Year=lubridate::ymd(df$Year,truncated=2L)

df$`Food & Beverages` <- as.numeric(as.character(df$`Food & Beverages`))
df$`Recreation & Culture` <- as.numeric(as.character(df$`Recreation & Culture`))
df$`Newspaper & Publications` <- as.numeric(as.character(df$`Newspaper & Publications`))
df$`Restaurants & Accomodation` <- as.numeric(as.character(df$`Restaurants & Accomodation`))
df$`Goods & Services` <- as.numeric(as.character(df$`Goods & Services`))

tsData = data.matrix(df)
tsData = ts(tsData ,start = c(2003,1),end = c(2018,9),frequency = 12)
tsData = tsData[,c(-1)]


#splitting the data
str(tsData)
autoplot(tsData) +
  ggtitle("Tourism and Hospitality Sector- Harmonised Consumer Price Index") +
  ylab("Moving 12 months average index in HICP (Base 2015=100)") +
  xlab("Year")

#Before and after data for food and beverage
f_1 <- window(tsData[,1], end = c(2011,6))
f_2 <- window(tsData[,1], start = c(2011,7))
t.test(f_1, f_2)
#reject null hypothesis. 


#-------T&H before 2011/07-----------------------
tsBefore2011 = window(tsData,start=c(2003,1),end=c(2011,6))
autoplot(tsBefore2011) +
  ggtitle("Tourism and Hospitality Sector- Harmonised Consumer Price Index , Before the 9% VAT implementation") +
  ylab("Moving 12 months average index in HICP (Base 2015=100)") +
  xlab("Year")

food1= window(tsBefore2011[,1],start=c(2003,1),end=c(2011,6))
recreation1= window(tsBefore2011[,2],start=c(2003,1),end=c(2011,6))
newspaper1= window(tsBefore2011[,3],start=c(2003,1),end=c(2011,6))
restaurants1= window(tsBefore2011[,4],start=c(2003,1),end=c(2011,6))
goods1= window(tsBefore2011[,5],start=c(2003,1),end=c(2011,6))

#-------T&H after 2011/07-------------------------

tsAfter2011=window(tsData,start=c(2011,7),end=c(2018,9))
autoplot(tsAfter2011) +
  ggtitle("Tourism and Hospitality Sector- Harmonised Consumer Price Index , After the 9% VAT implementation") +
  ylab("Moving 12 months average index in HICP (Base 2015=100)") +
  xlab("Year")

food2= window(tsAfter2011[,1],start=c(2011,7),end=c(2018,9))
recreation2= window(tsAfter2011[,2],start=c(2011,7),end=c(2018,9))
newspaper2= window(tsAfter2011[,3],start=c(2011,7),end=c(2018,9))
restaurants2= window(tsAfter2011[,4],start=c(2011,7),end=c(2018,9))
goods2= window(tsAfter2011[,5],start=c(2011,7),end=c(2018,9))



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

#----seasonal difference-----

nsdiffs(food1) #0
nsdiffs(recreation1) #0
nsdiffs(restaurants1) #0
nsdiffs(newspaper1)v #0
nsdiffs(goods1) #0
nsdiffs(food2) #0
nsdiffs(recreation2) #0
nsdiffs(restaurants2) #0
nsdiffs(newspaper2) #0
nsdiffs(goods2) #0
#
timeseriesseasonallyadjusted <- tsData- components.tsData$seasonal
plot(timeseriesseasonallyadjusted) # seasonaly adjusted 
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)
plot(tsstationary) #seasonaly adjusted and differenced
plot(diff(tsData,differences=1))
plot(aggregate.ts(tsData))
#Exploratory analysis food 
par(mfrow = c(1,2))


