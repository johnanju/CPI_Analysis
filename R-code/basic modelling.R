#--------------------------------------------------------
install.packages("xlsx")

#---------------------------------------------------------
library("xlsx")
library("zoo")
library("plyr")
library("fpp2")
library("lubridate")
library("fUnitRoots")
#--------------------------------------------------------

data = read.xlsx("CPM16_5.xlsx",1)
head(data)
df <- rename(data,c('Consumer.Price.Index.by.Month..Detailed.Sub.Indices.and.Statistic'='Year','NA.'='Food & Beverages', 'NA..1'='Recreation & Culture' , 'NA..2'='Newspaper & Publications', 'NA..3'='Restaurants & Accomodation','NA..4'='Goods & Services'))
df <- df[c(-1,-2),]
df <- df[c(-190:-210),]
head(df)
tail(df)
tsData = data.matrix(df)
tsData= ts(matrix(tsData,191,5) ,start = c(2003,1),end = c(2018,9),frequency = 12)
plot(tsData)
summary(tsData)

autoplot(tsData) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")


components.ts = decompose(tsData)
components.ts = decompose(tsData)
plot(components.ts)


