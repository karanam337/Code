remove(list=ls())

library(forecast)
library(ggplot2)
library(urca)

#install.packages('lubridate')
library(lubridate)

setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting')

temp=read.csv('claims.csv')
temp

claim=ts(temp[,2],start=decimal_date(ymd('1967-01-7')),frequency=365.25/7)

# hold out last 4 records. We have total of 2833 observations 
claimsHOLD=ts(temp[1:2829,2],start=decimal_date(ymd('1967-01-07')),frequency=365.25/7)
claimsHOLD

autoplot(claimsHOLD)

BC=BoxCox.lambda(claimsHOLD)
BC

transform=BoxCox(claimsHOLD,lambda=BC)
transform

autoplot(transform)

# AIC tends to select bigger models than BIC. Hence using AIC instead of BIC 
# as unit root test kind of takes bigger models for tests
summary(ur.df(transform,type='drift',lags=200,selectlags='AIC'))
nsdiffs(transform)

# ignoring the seasonal root (due to complexity of data and frequency) and for 
# non seasonal root -> not considering 1% test, we can reject the null and not do non seasonal differencing

# let us look at acf and pacf separately as we are dealing with higher lags and too much data for display
ggAcf(transform,lag.max=260)

ggPacf(transform,lag.max=260)

model=Arima(transform,order=c(2,0,2),seasonal=c(2,0,0))

# dynamic harmonic regression - instead of estimating models with seasonal AR dynamics
# use fourier terms for seasonal terms instead
# choose k value based on lowest AIC and BIC iteratively 
Arima(transform,order=c(2,0,2),xreg=fourier(transform,K=1))
modelHARM=Arima(transform,order=c(2,0,2),xreg=fourier(transform,K=26))

forecast(model,h=4,lambda=BC,biasadj=TRUE)
forecast(modelHARM,lambda=BC,biasadj=TRUE,xreg=fourier(transform,K=26,h=4))

# multi seasonality allows adding more than 1 type of seasonality
# here we are adding monthly and weekly seasonality 
claimsHOLDmulti=msts(temp[1:2829,2],start=decimal_date(ymd('1967-01-07')),
                   seasonal.periods=c(12, 365.25/7))
claimsHOLDmulti

# use stl if multiple seasons are not present
# let us de-seasonalize our data using mstl
# needs to windows to keep track of trend and seasonality. 
# how many observations in a row am I allowed to use in order to specify trend
# smaller the t.window, more abrupt the change in linear trend. 
# seasonality needs to be constant through out the sample -> periodic 

fit=mstl(claimsHOLDmulti,lambda='auto',t.window=13,s.window='periodic')
autoplot(fit)

#forecast(fit,lambda='auto',h=4,method='arima')
# is forecasting transformed values. Need to check
forecast(fit,lambda='auto',h=4,method='arima')

# another way is to use automated function in R using stlf
stlf(claimsHOLDmulti,lambda='auto',h=4,method='naive')
