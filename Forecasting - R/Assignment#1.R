
# Clear the environment 
remove(list=ls())

library(forecast)
library(ggplot2)

# Change the working directory
setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/Assignments/1')

# Read the input csv file
input=read.csv('pset1.csv')
input

# check the dimensions and head / tail of data
dim(input)
head(input)
tail(input)

# create a ts for entire data
data=ts(input$C02,start=c(1958,3),end=c(2021,1),frequency=12)
data

# create a ts for training by data holding out the last 6 observations 
# (August 2020 through January 2021)
train=ts(input$C02,start=c(1958,3),end=c(2020,7),frequency=12)
train

# Forecast using Naive method for a horizon of 6
nf=naive(train,h=6)
nf

# Forecast using Sample Mean method for a horizon of 6
sm=meanf(train,h=6)
sm

# Plot the data
autoplot(train,xlab='Years',ylab='CO2 level',main='Atmospheric CO2 levels over Time')
#plot(train,ylab='CO2 level',main='Atmospheric CO2 over Time')

# Plots indicating the seasonality 
monthplot(train,xlab='Months',ylab='CO2 level',main='Monthly plot of CO2 levels over the years')
seasonplot(train,xlab='Months',ylab='CO2 level',main='Seasonal plot of CO2 levels over the years')
ggseasonplot(train,main='Seasonal Plot')

# Decompose the data to view various aspects of data:
plot(decompose(train))

ETS=ets(train)
ETS
fit=forecast(ETS,h=6)
fit

# Use Holt-Winters method to model for additive trend and seasonality
hw=hw(train,h=6)
hw

# Forecasted values for horizon of 6, upper & lower confidence bands, actual data from Jan 2020
forecast_hw=hw$mean
upper95=hw$upper[,2]
lower95=hw$lower[,2]
actual=window(data,start=c(2020,1),end=c(2021,1))

forecast_hw
upper95
lower95
actual

# Generate plot of forecasted values along with confidence bands and actual values
graph_hw=ts.union(actual,forecast_hw,upper95,lower95)
autoplot(graph_hw,xlab='Years',ylab='CO2 level',main='Forecast vs Actual CO2 levels')+
        aes(color=series,linetype=series)+
        scale_color_manual(values=c('black','blue','red','red'))+
        scale_linetype_manual(values=c(1,2,3,3))  

accuracy(hw,data)

# Forecast into next 40 years with horizon as 40*12=480
forecast_hw_next40=hw(train,h=480)
#forecast_hw_next40

# Forecasted mean values for next 40 years and upper and lower confidence bands 
forecast_next40Yrs=forecast_hw_next40$mean
upper95_next40Yrs=forecast_hw_next40$upper[,2]
lower95_next40Yrs=forecast_hw_next40$lower[,2]

forecast_next40Yrs

# Generate plot of forecasted values along with confidence bands for next 40 years 
graph_next40Yrs=ts.union(actual,forecast_next40Yrs,upper95_next40Yrs,lower95_next40Yrs)
autoplot(graph_next40Yrs,xlab='Years',ylab='CO2 level',main='Forecast CO2 levels next 40 Yrs')+
  aes(color=series,linetype=series)+
  scale_color_manual(values=c('black','blue','red','red'))+
  scale_linetype_manual(values=c(1,2,3,3))  

# Forecasted mean values for next 15 years and upper and lower confidence bands 
forecast_next15Yrs=window(forecast_next40Yrs,start=c(2020,8),end=c(2035,7))
upper95_next15Yrs=window(upper95_next40Yrs,start=c(2020,8),end=c(2035,7))
lower95_next15Yrs=window(lower95_next40Yrs,start=c(2020,8),end=c(2035,7))

# Generate plot of forecasted values along with confidence bands for next 15 years 
graph_next15Yrs=ts.union(actual,forecast_next15Yrs,upper95_next15Yrs,lower95_next15Yrs)
autoplot(graph_next15Yrs,xlab='Years',ylab='CO2 level',main='Forecast CO2 levels next 15 Yrs')+
  aes(color=series,linetype=series)+
  scale_color_manual(values=c('black','blue','red','red'))+
  scale_linetype_manual(values=c(1,2,3,3))  
