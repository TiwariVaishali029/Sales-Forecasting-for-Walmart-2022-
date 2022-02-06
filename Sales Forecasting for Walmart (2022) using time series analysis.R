###Forecasting with R###

#installing Packages
install.packages('forecast')
install.packages('tsutils')
install.packages('MAPA')

#Loading necessary libraries
library(forecast)
library(tsutils)
library(MAPA)

#Loading time series
file<- ts(scan("sales.txt"),start=c(2016,1), frequency=4)
file1<-ts(scan("salesout.txt"),start=c(2021,1), frequency=4)
#Storing time series to be explored in variable "y"
y<-file
y1<-file1

#Set horizon
h<-length(y1)

#Plotting time series
plot(y)

#----trend---#
# Let us look for trend in the data by calculating the centered moving average
cma<-cmav(y, outplot=1)
print(cma)

#We can clearly see there is an upward trend in time series

#we can test weather the trend is significant by using coxstuart
coxstuart(cma)


#----Season----#
#We can test for seasonality visually by producing a seasonal plot
seasplot(y)


#----Decomposition----#
#we can perform classical decomposition using decomp function
decomp(y, outplot=1)


#---------Simple Moving Average------#
sm<-ma(y, order=4)
plot(sm, col='red')

#-------Naive------#
#replicate the last value h times
f.naive<- rep(tail(y,1),h)

f.naive<-ts(f.naive,start=start(y1),frequency=frequency(y1))
f.naive

#now we can plot all element
ts.plot(y,y1,f.naive,col=c("blue","blue","red"))

#------------Seasonal Naive-----------#
f.SNaive <- rep(tail(y,frequency(y)),ceiling(frequency(y)/h))
f.SNaive <- ts(f.SNaive, start=start(y1), frequency=frequency(y1))
ts.plot(y,y1,f.SNaive, col=c("blue","blue","red"))

#------Exponential Smoothing----#
#Exponential Smoothing- we will use the ets function from the forecast package
fit.ets<-ets(y)
print(fit.ets)

#forecasting using exponential smoothing
f.ets<-forecast(fit.ets, h=h)
print(f.ets)
plot(f.ets)

lines(y1, col='red')
ets(y,model="AAA")

plot(fit.ets)
accuracy(fit.ets)

#--------------Holt Winters-------------#

print("Holt Winters")
y= ts(scan("sales.txt"),start=c(2016,1), frequency=4)
autoplot(y,xlab="Year",ylab="Sales",main=" Holt Winters Method")
fit= HoltWinters(y)
fit
plot(fit)
f.holt<-forecast(fit,4)
f.holt
plot(forecast(fit,4))
summary(fit)

#----------Arima-------#


tsData = ts(y, start = c(2016,1), frequency = 4)

#We can use the following R code to find out the components of this time series
components.ts = decompose(tsData)
plot(components.ts)

#R code for unit root test
install.packages('fUnitRoots')
library("fUnitRoots")
urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsData, differences=1)
plot(tsstationary)


#code to run the acf() and pacf() commands
acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)

fitARIMA <- arima(tsData, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
library(lmtest)
coeftest(fitARIMA) 

confint(fitARIMA)

# auto.arima to use the best model for forecasting
auto.arima(tsData, trace=TRUE) 
f.arima<-predict(fitARIMA,n.ahead = 4)
f.arima

#Finally let us evaluate the forecasts
#Note that we use only the $mean from the predictions that contains prediction intervels
f.all<-cbind(f.naive,f.SNaive,f.ets$mean,f.arima$mean,f.holt)

#find the number of forecasts
K<-dim(f.all)[2]

#Replicate test set k times
#We can do this quickly by using the function tcrossprod
#we give it avector of k ones and the test set that are matrix multiplied together
#This is equivalent to rep(1,k)%%t(y1)
y.all<-t(tcrossprod(rep(1,K),y1))


#Calculate errors
E<-y.all-f.all

#Summarise errors
#ME is mean by column
ME<-apply(E,2,mean)
  
#MAE
MAE<-apply(abs(E),2,mean)

#MAPE
MAPE<- apply(abs(E)/y.all,2,mean)*100


#Let us collate the results in a nice table
res<-rbind(ME,MAE,MAPE)

  rownames(res)<- c("ME","MAE","MAPE")
  colnames(res)<- c("naive","snaive","ets","arima","holt")
  print(round(res,2))

