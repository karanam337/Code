remove(list=ls())

library(forecast)
library(ggplot2)
library(urca)

setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/1 - Jan 20')
temp=read.csv('day1.csv')
temp

sw=ts(temp[,2],start=c(1993,1),frequency=4)
swHOLD=ts(temp[,2],start=c(1993,1),end=c(2019,4),frequency=4)
swHOLD

autoplot(swHOLD)

lamSW=BoxCox.lambda(swHOLD)
transform=BoxCox(swHOLD,lambda=lamSW) 
autoplot(transform)

summary(ur.df(transform,type='trend',lags=17,selectlags='AIC'))
# fail to reject null, d=1

nsdiffs(transform)
nsdiffs(diff(transform))

# So considerable evidence that D=1
# As a robustness check, we can reapply the unit root test on sesonally differenced data

summary(ur.df(diff(transform,4),type='drift',lags=17,selectlags='AIC'))
# failure to reject with a 5% test state. -2.7499>-2.88
# proceed with d=1 and D=1

ggtsdisplay(diff(diff(transform,4)),lag.max=21)

# choose arima parameters
Arima(transform,order=c(1,1,0),seasonal=c(2,1,0))
# BIC 218.28

Arima(transform,order=c(1,1,0),seasonal=c(0,1,1))
# BIC 205.38

Arima(transform,order=c(0,1,1),seasonal=c(0,1,1))
# BIC 205.23

Arima(transform,order=c(2,1,0),seasonal=c(2,1,2))
# BIC 221.77

Arima(transform,order=c(0,1,0),seasonal=c(0,1,1))
# BIC 201.52

Arima(transform,order=c(0,1,2),seasonal=c(0,1,2))
# BIC 201.52

model=Arima(transform,order=c(0,1,2),seasonal=c(0,1,2))
checkresiduals(model)

fore=forecast(model,h=3,lambda=lamSW,biasadj=TRUE)
fore
window(sw,start=c(2020,1))

accuracy(fore,sw)

passHOLD=ts(temp[,4],start=c(1993,1),end=c(2019,4),frequency=4)
passHOLD

# add passHOLD as an exogenous regressor -> if we have 2 like passHOLD and u
model=Arima(transform,order=c(0,1,2),seasonal=c(0,1,2),xreg=passHOLD)
model

# add passHOLD as an exogenous regressor along with another variable u 
#model=Arima(transform,order=c(0,1,2),seasonal=c(0,1,2),xreg=cbind(passHOLD,u))

forecast(model,h=3,lambda=lamSW,biasadj=TRUE,xreg=c(40000,8000,15000))

install.packages('vars')
library('vars')