getwd()
require("xlsx")

#Importing Data
time<-read.xlsx(file="E:\\R\\final case study +interview preparation\\unrar final case\\files\\UK for R1.xlsx",sheetIndex = 1)

#converting into time series data
tm<-ts(time$Total, start = c(1996,1),end = c(2005,4),frequency = 4)

#plot.ts(time$Total)

plot(tm)

#to check seasonal,trend & remainder in data# 
tm2 <- stl(tm, s.window = "periodic")
tm2
plot(decompose(tm2, type = c("multiplicative")))


require("forecast")
#Exponential Smoothing # 

#simple exponential - models level
case1<- HoltWinters(tm, beta=FALSE, gamma=FALSE)
ls(case1)
accuracy(case1$fitted,tm) #MAPE~18.10%


# double exponential - models level and trend
case2 <- HoltWinters(tm, gamma=FALSE)
accuracy(case2$fitted, tm) #MAPE~23.5%

# triple exponential - models level, trend, and seasonal components
case3<-HoltWinters(tm)
accuracy(case3$fitted,tm) #MAPE~2.9%

forecast(case3,4)  #forecasting values

?ets()
#predicted accuracy#
case4 <- ets(tm)
accuracy(case4$fitted, tm)
forecast(case4, 4)

#ARIMA#
case5 <- auto.arima(tm)
accuracy(case5)
forecast(case5, 4)
plot(forecast(case5, 4))
