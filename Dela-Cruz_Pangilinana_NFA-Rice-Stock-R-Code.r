
#AMAT 132- Introductory Forecasting
#NFA Rice Monthly Rice Stock from 2001 to April 2022

#Name: Bhea M. Dela Cruz & Myrene Pangilinan



library(readxl)
stock <- read_excel("C:/Users/Gladys/Desktop/FORECASTING PROJECT- FILES/NFA-STOCK 2001-2022.xlsx", 
                    +     range = "A1:C257")
View(stock)
head(stock)
tail(stock)
length(stock)

#libraries
library(ggplot2)
library(urca)
library(forecast)
library(tseries)


stock_ts<-msts(stock$Stock,seasonal.periods = c(12),start=c(2001,01))
autoplot(stock_ts)+ ggtitle("NFA Rice Monthly Stock")+xlab("Time")+ylab("in thoussand metric tons")



#Times Series Decomposition
decom<-decompose(stock_ts)
plot(decom)
plot(decompose(stock_ts,type="multiplicative"))

#Stationarity
adf.test(stock_ts) #non stationary

ndiffs(stock_ts) #return [1]

#Differencing
adf.test(diff(stock_ts)) #now stationary

#ACF and PACF
plot(acf(diff(stock_ts)),main="ACF")
plot(pacf(diff(stock_ts)),main="PACF")

#fit model ARIMA(0,1,3)
fit_mod1<-Arima(stock_ts,order=c(0,1,3))
fit_mod1

#Auto Arima
fit_mod2<-auto.arima(stock_ts,seasonal=FALSE)
fit_mod2

#ARIMA(1,1,0) has lesser AICc than ARIMA(0,1,3)
#adopt ARIMA(1,1,0)
#Residuals
checkresiduals(fit_mod2)

#Residuals shows few significant spikes with variations in timeplot

#Strengthen Arima
fit_mod3<-auto.arima(stock_ts,seasonal = FALSE,stepwise=FALSE,approximation = FALSE)
fit_mod3

#ARIMA(2,1,3)lesser AICc
checkresiduals(fit_mod3)
#the ACF plot nearly looks like a white noise
#better model

#Choose ARIMA(2,1,3) to forecast

autoplot(forecast(fit_mod3))

#Forecast
NFA=forecast(fit_mod3, level=c(95),h=3*12)

autoplot(NFA)+ylab("in thousand metric tons")+ggtitle("Philippine NFA Stock 3 years")



#We will aslo try other methods

#Forecast Accuracy for other methods
stock2 <- window(stock_ts,start=2001,end=c(2017,12))
meanfit1 <- meanf(stock2,h=3*12)
naivefit2 <- rwf(stock2,h=3*12)
snaivefit3 <- snaive(stock2,h=3*12)
autoplot(window(stock_ts, start=2001)) +
  autolayer(meanfit1, series="Mean", PI=FALSE) +
  autolayer(naivefit2, series="Naïve", PI=FALSE) +
  autolayer(snaivefit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Thousand metric tons") +
  ggtitle("Forecasts for monthly NFA Rice Stock") +
  guides(colour=guide_legend(title="Forecast"))

#Accuracy
stock3 <- window(stock_ts, start=2018)
accuracy(meanfit1, stock3)
accuracy(naivefit2, stock3)
accuracy(snaivefit3, stock3)

#Among the 3, the naive is more accurate for the dataset 
#with the least RMSE and MAPE

#[End Code]





