remove(list=ls())
setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/9 - Apr 7')
temp=read.csv("liquor.csv")
liqourHOLD=ts(temp[,2],start=c(1992,1),end=c(2020,10),frequency=12)
liqour=ts(temp[,2],start=c(1992,1),frequency=12)
plot(liqour)
lamLIQUOR=BoxCox.lambda(liqourHOLD)
lamLIQUOR
transform=BoxCox(liqourHOLD,lamLIQUOR)
plot(transform)
summary(ur.df(transform,type="trend",lags=37,selectlags="AIC")) 
nsdiffs(transform)
nsdiffs(diff(transform))
plot(diff(transform,12))
plot(decompose(diff(transform,12)))
summary(ur.df(diff(transform,12),type="drift",lags=37,selectlags="AIC"))
ggtsdisplay(diff(diff(transform,12)),lag.max=60)

auto.arima(liqourHOLD)
# p=2 onwards, P=2 onwards
# d=1, D=1
# q=2 onwards, Q=2 onwards

Arima(liqourHOLD,order=c(2,1,2),seasonal=c(2,1,2))

