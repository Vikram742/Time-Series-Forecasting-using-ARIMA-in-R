#set working directory and read the data
mumbai<- read.csv("rainfall.csv")
#we are only interested in two columns i.e datetime, temp.
names(mumbai)
mumbai1<- mumbai[,c(1:2)]
#data cleaning
sum(is.na(mumbai1))
str(mumbai1)
#datetime column is character vector which needs to be changed to date vector
library(lubridate)
mumbai1$datetime<-parse_date_time(mumbai1$datetime , orders = c("%d-%m-%Y", "%d/%m/%Y"))
str(mumbai1$datetime)
mumbai1$datetime<-as.Date(mumbai1$datetime)
#Run libraries
library(forecast)
library(tseries)
#check whether the data is statitonary or not. ARIMA works only with stationary data where the mean and variance are constant.
plot.ts(mumbai1$temp)
#While looking at the plot the data looks stationary but we should cross check the same
adf.test(mumbai1$temp)
#p value is 0.01 which is less than that of alpha. This shows that the data is stationary.
# apply auto.arima to best fit the ARIMA model
auto.arima(mumbai1$temp)
# ARIMA(3,0,4) with non-zero mean i.e p = 3 , d = 0 , q = 4
# let's create the model
model<-arima(mumbai1$temp, order = c(3,0,4))
model

# Diagnostic check options means we have to check the residuals. Basically it means what is left over after
# fitting the model.Residuals are important to check if the model has adequately captured the data or not
# There should be no corrlealtion amongst the residuals
# Residuals should have zero mean
# Residuals should be normally distributed
xyz<- residuals(model)
#check for correlation
acf(xyz) # bar touches the blue line around lag 6
# check for zero mean
plot.ts(xyz) # it shows mean is constant 
# check for normal distribution
gghistogram(xyz)
# forecast the time series data
predictions<- forecast(model, h=15)
predictions # forecasted values from 16th Nov 2020 till 30th Nov 2020 along with the lower and upper limit
# at 80% and 95% confidence level
plot(predictions)
# we want to compare the last 5 actual temperature vs predicted temperature to see the difference
model<-arima(mumbai1$temp[1:1776],order=c(3,0,4))
predictions<-forecast(model, h=5)
predictions
tail(mumbai1$temp,5)
# Check for accuracy
accuracy(model)
#RMSE is 0.795, MAPE is 2.11 (accuracy is 97.89%), MAE is 0.587 which means that the predicted value deviates
# from the actual value only by 0.587. We can say that the model is significant.


