library(forecast)
library(urca)
library(ggplot2)
library(vars)

setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting')
temp=read.csv('April14part2.csv')

# data is seasonally adjusted. VAR is better with data that doesnt have seasonality

exportFULL=ts(temp[,2],start=c(1991,1),frequency=12)
growFULL=ts(temp[,3],start=c(1991,1),frequency=12)
rupeeFULL=ts(temp[,4],start=c(1991,1),frequency=12)

# are all variables integrated of the same order?  (are they stationary)
autoplot(exportFULL)
autoplot(growFULL)
autoplot(rupeeFULL)

summary(ur.df(exportFULL,type='drift',lags=20,selectlags='AIC'))
# considering 5% -> -3.2 < -2.87 -> reject NULL, hence I(0)

summary(ur.df(growFULL,type='drift',lags=20,selectlags='AIC'))
# considering 5% -> -3.2682 < -2.87 -> reject NULL, hence I(0)

summary(ur.df(rupeeFULL,type='drift',lags=20,selectlags='AIC'))
# considering 5% -> -0.5246 > -2.87 -> fail to reject NULL, hence I(1)

# even if we consider with trend:
summary(ur.df(rupeeFULL,type='trend',lags=20,selectlags='AIC'))
# considering 5% -> -2.0344 > -3.42 -> fail to reject NULL, hence I(1)

# when we difference the data, we lose the first observation. 
# Hence all data will start from Feb 1991 as a result. 

# below is to make series start from 1991 feb

export=window(exportFULL,start=c(1991,2))
grow=window(growFULL,start=c(1991,2))
rupeeGROW=diff(rupeeFULL)
autoplot(rupeeGROW)

Y=cbind(export,grow,rupeeGROW)
VARselect(Y,lag.max=13,type='const')

# let us chose p=2 here as SIC (SC) is lowest for least parsimonious model. 
# understand AIC is lowest for p=12

VARmodel=VAR(Y,p=2,type='const')
VARmodel

foreVAR=forecast(VARmodel,h=6)
foreVAR

forecast_export=foreVAR$forecast$export
forecast_grow=foreVAR$forecast$grow
foreVAR$forecast$rupeeGROW

# diffinv needs past values of the level of the variable upon which the forecast is built
diffinv(foreVAR$forecast$rupeeGROW$mean,xi=rupeeFULL[length(rupeeFULL)])

# Neural Network
# what value should i use for p and P
# How many nodes to use

ggtsdisplay(exportFULL,lag.max=30)
# for export data, suppose I set p=2 (SIC from VAR) 
# P=2 (on basis on PACF)
NETmodel=nnetar(exportFULL,p=2,P=2)
NETmodel

NETmodel=nnetar(exportFULL,p=2,P=2,size=3,repeats=10)
NETmodel

# PI is for boot strapping and also if you need confidence bands 
forecast(NETmodel,h=4,PI=TRUE)

# with exogenous regressors
netmodelexo=nnetar(export,p=2,P=2,xreg=rupeeGROW)
forecast(netmodelexo,h=4,xreg=c(-1,-1,-1,-1),PI=TRUE)
