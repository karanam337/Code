# ARIMA model

remove(list=ls())

library(forecast)
library(ggplot2)

# to create white noise
e = rnorm(5000)
e
ggtsdisplay(e,lag.max=30)

#AR(1) model
# yt=phi*y_{t-1}+et
# initialization condition: 
# y1=phi*y0+e1

# set y1=0 as we dont have y0. We can also set y0=0 as well. 
# y2=phi*y1+e2

y=0
y

# if we need to initialize 100 values to zero, then use y=(1:100)*0
y=(1:100)*0
y

for (t in 2:5000)
{
  y[t]=0.9*y[t-1]+e[t]
}
y

ggtsdisplay(y,lag.max=20)

install.packages('urca')
library(urca)

setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/8 - Mar 31')
temp=read.csv('gdp.csv')
gdp=ts(temp[,2],start=c(1959,4),frequency=4)

# quarter over quarter growth rates in gdp
growthQOQ=diff(log(gdp))*4*100

# log(gdp{t})-log(gdp_{t-1}) -> this is what differencing is doing. 
# subtracting current value from previous value
# same as above: 
#growthQOQ=diff(log(gdp),1)*400

autoplot(growthQOQ)

# year on year growth. Note we are not multiplying using 4 like in quarterly
# as this is already annualized data
growthYOY=diff(log(gdp),4)*100
autoplot(growthYOY)
growthYOY

# simple simulation on AR(1)
e=rnorm(5000)
AR1=0
for (t in 2: 5000)
{
  AR1[t]=0.9*AR1[t-1]+e[t]
}

# ACF at lag k should be equal to 0.9^k

ggtsdisplay(AR1,lag.max=40)
checkresiduals(AR1)
# for MA(1) simulation, we will use same white noise as above
MA1=0
for (t in 2:5000)
{
  MA1[t]=e[t]+0.9*e[t-1]
}
ggtsdisplay(MA1,lag.max=15)
checkresiduals(MA1)

# now let us look at annual growth rate of gdp data
# 1) A unit root test would yield evidence against a unit root. I=0 (GDP growth)
# Unit root test is used to ask what is the integration order I in ARIMA
# 2) Guesses about p & q

ggtsdisplay(growthYOY,lag.max=20)

# AR(3): Model theoretically possesses an ACF that is never 0. PACF is exactly 0 after 3 lags
# ARMA(1,1): Model theoretically possesses an ACF with smooth exponential decay (ACF is never theoretically zero)
#            With a moving average component, the PACF is expected never to be zero 

# 3) Estimate your guesses
Arima(growthYOY,order=c(3,0,0))
Arima(growthYOY,order=c(1,0,1))
Arima(growthYOY,order=c(1,0,0))
Arima(growthYOY,order=c(3,0,4))

# AR(3) has the lowest BIC value
# infact ARMA(3,4) has lowest BIC

# 4) Run diagnostic tests
# do my residuals display any characteristics that are inconsistent with the properties of white noise 

# save the model and check for residuals
model=Arima(growthYOY,order=c(3,0,0))
checkresiduals(model)

model1=Arima(growthYOY,order=c(3,0,4))
checkresiduals(model1)

# unit root test (dickey fuller test) -> does my data provide evidence of unit root or not?
# Null hypothesis is that there is a unit root

summary(ur.df(growthYOY,type='drift',lags=20,selectlags='AIC'))


e=rnorm(5000)
y=0
for (t in 2:5000)
{
  y[t]=1.02*y[t-1]+e[t]
}
plot(y)

################
library(urca)
library(forecast)
library(ggplot2)

remove(list=ls())

setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/9 - Apr 7')
temp=read.csv('grocery.csv')
grocery=ts(temp[,2],start=c(1992,1),frequency=12)
groceryHOLD=ts(temp[,2],start=c(1992,1),end=c(2020,9),frequency=12)

grocery
groceryHOLD

# plot reveals evidence on non linear trend. We can consider a box cox transformation. 
# Simplest transformation is logarithmic transformation 
autoplot(groceryHOLD)

# Here we consider using the value of lambda associated with the single parameter box-cox
# transformation as selected by R to modify our data
l=BoxCox.lambda(groceryHOLD)
l

# The selected lambda value is far away from 1, implying a transformation is likely useful. 
# The value is also somewhat far from 0 (when lambda=0, we get a logarithmic transformation)

# We save the lambda selected by R and use it to transform the data
lambdaGROCERY=BoxCox.lambda(groceryHOLD)
transform=BoxCox(groceryHOLD,lambda=lambdaGROCERY)

autoplot(transform)
# the plot of data shows a series that appears to be more manageable 
# Trend appears to be roughly linear, and volatility through the series has been reduced
# note this transformation is not related to covariance stationary transformation

######## simulate for various values of phi in AR(1)
e=rnorm(5000)
y=0
#phi=1.02(explosive)
for (t in 2:5000)
{
  y[t]=1.02*y[t-1]+e[t]
}
plot(y)

y=0
#phi=1.01(still explosive)
for (t in 2:5000)
{
  y[t]=1.01*y[t-1]+e[t]
}
plot(y)

y=0
#phi=1(random walk, non stationary, but realistic looking simulation)
for (t in 2:5000)
{
  y[t]=1.0*y[t-1]+e[t]
}
plot(y)

y=0
#phi=0.9  
for (t in 2:5000)
{
  y[t]=0.9*y[t-1]+e[t]
}
plot(y)

########
# As the transformed data includes trend, it needs to be included in the unit root test

summary(ur.df(transform,type='trend',lags=37,selectlags='AIC'))

# Check if seasonal differencing is needed. 
# nsdiffs function provides indication whether seasonal differencing may be needed
nsdiffs(transform)

# does seasonal differencing still seem appropriate after first differencing the data (d=1)
nsdiffs(diff(transform))

# Confirm if the previous unit root test was impacted by presence of seasonal unit root

# plot of seasonal differenced data shows trend has likely been eliminated
plot(diff(transform,12))
autoplot(decompose(diff(transform,12)))
# Apply the unit root test again on this seasonally differenced data
# Since trend has been eliminated, we run the test with drift only

summary(ur.df(diff(transform,12),lags=37,type='drift',selectlags='AIC'))

# based on the previous unit root test and current test results, we conclude that 
# both non seasonal and seasonal differencing in needed as test statistic is > critical value

ggtsdisplay(diff(diff(transform,12)),lag.max=40)

# based on the reviews on acf and pacf plots, let us make guesses on values of 
# p,q,P,Q and estimate models

Arima(transform,order=c(2,1,0),seasonal=c(2,1,0))
Arima(transform,order=c(2,1,1),seasonal=c(2,1,0))
Arima(transform,order=c(2,1,2),seasonal=c(2,1,2))
Arima(transform,order=c(2,1,2),seasonal=c(2,1,0))

# The first model has lowest AIC/BIC (per professor). However I see 3rd model has lowest BIC
# let us save this model and analyze its residuals using ggtsdisplay
model=Arima(transform,order=c(2,1,0),seasonal=c(2,1,0))

# considering lag.max=49 as for 4 years + 1 to cover for seasonality
ggtsdisplay(model$residuals,lag.max=49)

# review of ACF and PACF plots indicate significant coeff's at seasonal lags. 
# consider increasing seasonal AR and seasonal MA pieces.

Arima(transform,order=c(2,1,0),seasonal=c(4,1,0))
Arima(transform,order=c(2,1,0),seasonal=c(2,1,2))
Arima(transform,order=c(2,1,0),seasonal=c(2,1,3))

# the 2nd model produces lowest BIC while 3rd model has lowest AIC. 
# let us consider 3rd model -> either one seems to be reasonable. 

model=Arima(transform,order=c(2,1,0),seasonal=c(2,1,3))
model

# we can examine residuals again using the below
ggtsdisplay(model$residuals,lag.max=49)

# Finally perform diagnostic test using checkresiduals
# almost all coeffs are within the 95% confidence bands (except may be lag 3), there
# is little evidence of residual correlation 
checkresiduals(model)

# forecast for next 4 months
# need to use original value of lambda along with bias adjustment to inform R to correct
# for issues related to Jensen's inequality 
fore=forecast(model,lambda=lambdaGROCERY,biasadj=TRUE,h=4)
fore

accuracy(fore)
accuracy(fore,grocery)

#accuracy(fore,BoxCox(grocery,lambda=BoxCox.lambda(grocery)))

# formally running Ljung box test. 
#model=Arima(transform,order=c(2,1,0),seasonal=c(2,1,3))
# fitdf=2+2+3
Box.test(model$residuals,lag=37,type='Ljung-Box',fitdf=7)
Box.test(model$residuals,lag=37,type='Ljung-Box')
