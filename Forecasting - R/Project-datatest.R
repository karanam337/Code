library(forecast)
library(ggplot2)

setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/Project')
temp=read.csv('HSN1FNSA.csv')

txpop=ts(temp[,2],start=c(1963,1),frequency=12)
txpop

autoplot(txpop)
autoplot(log(txpop))

etsmodel=ets(txpop)
etsmodel

etsmodel1=ets(log(txpop))
etsmodel1

seasonplot(log(txpop))
monthplot(log(txpop))
ggseasonplot(log(txpop))

autoplot(decompose(log(txpop)))
autoplot(decompose(txpop))
auto.arima(log(txpop))
