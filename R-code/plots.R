
#Plots

autoplot(tsData) +
  ggtitle("Tourism and Hospitality Sector- Harmonised Consumer Price Index") +
  ylab("Moving 12 months average index in HICP (Base 2015=100)") +
  xlab("Year")

autoplot(tsBefore2011) +
  ggtitle("Tourism and Hospitality Sector- Harmonised Consumer Price Index , Before the 9% VAT implementation") +
  ylab("Moving 12 months average index in HICP (Base 2015=100)") +
  xlab("Year")

autoplot(tsAfter2011) +
  ggtitle("Tourism and Hospitality Sector- Harmonised Consumer Price Index , After the 9% VAT implementation") +
  ylab("Moving 12 months average index in HICP (Base 2015=100)") +
  xlab("Year")

#Seasonal Decomposition of Time Series by Loess
plot(stl(food1,"per")) # highly affected by the recention in 2009,seasonal
plot(stl(recreation1,"per")) # seasonal, with upward trend
plot(stl(newspaper1,"per")) # seasonal with upward trend
plot(stl(restaurants1,"per")) # seasonal with upward trend
plot(stl(goods1,"per")) #seasonal with exponential trend

plot(stl(food2,"per"))
plot(stl(recreation2,"per"))
plot(stl(newspaper2,"per"))
plot(stl(restaurants2,"per"))
plot(stl(goods2,"per"))

#seasonal plot
ggseasonplot(tsData[,1],season.labels = NULL, year.labels = TRUE,year.labels.left = TRUE,col = rainbow(12))
seasonplot(food1, 12,season.labels = NULL, year.labels = TRUE,year.labels.left = TRUE,col = rainbow(12))
ggseasonplot(tsData[,2],season.labels = NULL, year.labels = TRUE,year.labels.left = TRUE,col = rainbow(12))
ggseasonplot(tsData[,3],season.labels = NULL, year.labels = TRUE,year.labels.left = TRUE,col = rainbow(12))
ggseasonplot(tsData[,4],season.labels = NULL, year.labels = TRUE,year.labels.left = TRUE,col = rainbow(12))
ggseasonplot(tsData[,5],season.labels = NULL, year.labels = TRUE,year.labels.left = TRUE,col = rainbow(12))

#Lagplot
gglagplot(tsData)

par(mfrow = c(2,3))
Acf(tsBefore2011)
Acf(tsAfter2011)

# autoplot(food1)+
#   autolayer(((meanf(food1,h=11))+ series ="Mean", PI=FALSE)+
#               autolayer((naive(food1,h=11))+series="Naive", PI= FALSE)+
#               autolayer((snaive(food1,h=11)),series = "seasonal naive", PI=FALSE)+
#               ggtitle))
