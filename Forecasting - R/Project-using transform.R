remove(list=ls())

library(forecast)
library(ggplot2)
library(urca)

setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/Project')

temp=read.csv('HSN1FNSA.csv')
temp

hsold=ts(temp[,2],start=c(1963,1),frequency=12)
hsold

hsoldHOLD=ts(temp[,2],start=c(1963,1),end=c(2020,10),frequency=12)
hsoldHOLD

autoplot(hsoldHOLD)

hsoldLAMBDA=BoxCox.lambda(hsoldHOLD)
hsoldLAMBDA

transform=BoxCox(hsoldHOLD,lambda=hsoldLAMBDA)

autoplot(transform)

summary(ur.df(transform,type='trend',lags=37,selectlags='AIC'))

nsdiffs(transform)

autoplot(diff(transform,12))

summary(ur.df(diff(transform,12),type='drift',lags=37,selectlags='AIC'))

autoplot(decompose(transform))

monthplot(transform)
ggseasonplot(transform)
seasonplot(transform)

ggtsdisplay(diff(transform,12),lag.max=49)

Arima(transform,order=c(2,0,0),seasonal=c(1,1,0))
Arima(transform,order=c(2,0,1),seasonal=c(1,1,0))
Arima(transform,order=c(2,0,1),seasonal=c(1,1,1))
Arima(transform,order=c(2,0,0),seasonal=c(2,1,0))
Arima(transform,order=c(2,0,1),seasonal=c(2,1,0))
Arima(transform,order=c(2,0,1),seasonal=c(2,1,1))

model_sarima=Arima(transform,order=c(2,0,1),seasonal=c(1,1,1))
model_sarima

ggtsdisplay(model_sarima$residuals,lag.max=37)

checkresiduals(model_sarima)

fore_sarima=forecast(model_sarima,h=4,lambda=hsoldLAMBDA,biasadj=TRUE)
fore_sarima

forecast_sarima=fore_sarima$mean
actual_hsold=window(hsold,start=c(2020,1))
upper95_sarima=fore_sarima$upper[,2]
lower95_sarima=fore_sarima$lower[,2]

graph_plot=ts.union(actual_hsold,forecast_sarima,upper95_sarima,lower95_sarima)
autoplot(graph_plot,xlab='Years',ylab='Houses Sold (in thousands)',main='Forecast vs Actual - SARIMA')+
  aes(color=series,linetype=series)+
  scale_color_manual(values=c('black','blue','red','red'))+
  scale_linetype_manual(values=c(1,2,3,3)) 

model_ETS=ets(transform)
model_ETS
