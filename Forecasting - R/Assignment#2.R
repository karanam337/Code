# Clear the work environment
remove(list=ls())

# Load required libraries
library(forecast)
library(ggplot2)
library(urca)

# Change the work directory and read the input file
setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/Assignments/2')
temp=read.csv('esales.csv')
temp

# Complete data
Esales=ts(temp[,2],start=c(1992,1),frequency=12)
Esales

# Data for training excluding last 4 observations
EsalesHOLD=ts(temp[,2],start=c(1992,1),end=c(2020,9),frequency=12)
EsalesHOLD

# Let us plot the EsalesHOLD data
autoplot(EsalesHOLD)
# The plot reveals non linear trend in data and also data series is volatile

# Let us use Logarithmic transformation on this data as mentioned in the problem set
# to restrict the forecast values to positive values only
transform=log(EsalesHOLD)

# Plot of log transformed data although not perfectly linear, seems more manageable
# The volatility in data is significantly reduced when compared with original series
autoplot(transform)

#summary(ur.df(transform,type='trend',lags=30,selectlags='AIC'))

# The problem set mentions that a seasonal unit root is present and below confirms same
nsdiffs(transform)

# Plot of the seasonally differenced data. As its monthly data s=12
# Looks like trend in data is eliminated
autoplot(diff(transform,12))

# Let us perform a unit root test (ADF) on the seasonally differenced data to 
# check for the presence a non seasonal unit root in the data
# Since trend is removed, we will run the test with drift only 
summary(ur.df(diff(transform,12),type='drift',lags=30,selectlags='AIC'))

# NULL: There is a non seasonal unit root present
# Reject NULL if test statistic < Critical value

# For 1% critical value  : -2.9218 > -3.44 -> Fail to reject NULL 
# For 5% critical value  : -2.9218 < -2.87 -> Reject NULL 
# For 10% critical value : -2.9218 < -2.57 -> Reject NULL

# Although results are not consistent, I consider to reject the NULL after examining
# the conventional 5% and 10% test size and conclude non-seasonal differencing is not required
# Hence d=0 from above test and D=1 (mentioned in problem set that seasonal unit root exists)

# Let us review the ACF and PACF plots using ggtsdisplay on seasonally differenced data
# to make guesses for SARIMA terms (p,q) and (P,Q). 
ggtsdisplay(diff(transform,12),lag.max=30)
#ggtsdisplay(diff(diff(transform,12)),lag.max=30)

# Based on review of ACF and PACF plots, we conclude below: 
#	p=4 onwards, P=2 onwards
#	d=0 and D=1
#	q=0 onwards, Q=0 onwards

#Let us estimate below models with guesses on (p,0,q)x(P,1,Q): 
Arima(transform,order=c(4,0,0),seasonal=c(2,1,0))
Arima(transform,order=c(4,0,0),seasonal=c(3,1,0))
Arima(transform,order=c(5,0,0),seasonal=c(2,1,0))
Arima(transform,order=c(5,0,0),seasonal=c(3,1,0))
Arima(transform,order=c(4,0,1),seasonal=c(2,1,1))
Arima(transform,order=c(4,0,2),seasonal=c(3,1,2))
Arima(transform,order=c(5,0,1),seasonal=c(2,1,1))
Arima(transform,order=c(5,0,2),seasonal=c(3,1,2))

# SARIMA(5,0,2)x(3,1,2) model has the lowest AIC and BIC among all estimated models
# It may be the best fit model, let us save that as our model 
model=Arima(transform,order=c(5,0,2),seasonal=c(3,1,2))

# Analyze the residuals using ggtsdisplay for selected model
ggtsdisplay(model$residuals,lag.max=30)

# With almost all the ACF and PACF coefficients being statistically insignificant
# (except for lag 23), we see there is small evidence of residual correlation.
# Hence we can conclude that the residuals lack serial correlation

# Let us also perform a diagnostic test using checkresiduals to see if our residuals
# display any characteristics that are inconsistent with white noise. 
checkresiduals(model)

# As before we see all ACF coefficients are statistically insignificant (except for lag23)
# and model is a reasonable fit for us to work with.

# Let us use the selected model to forecast 4-step ahead (Oct 2020 thru Jan 2021)
# As the original data is log transformed, we should undo that transformation 
# during forecasting by specifying lambda=0 (for log transformation).
# Also biasadj=TRUE parameter should handle for issues related to Jensen's inequality 

fore=forecast(model,lambda=0,biasadj=TRUE,h=4)
fore

# Forecasted values for horizon of 4 and actual data from Jan 2020
forecast_Esales=fore$mean
actual_Esales=window(Esales,start=c(2020,1))

# Generate plot of forecasted values along with actual values
graph_model=ts.union(actual_Esales,forecast_Esales)
autoplot(graph_model,xlab='Years',ylab='Esales',main='Forecast vs Actual Esales')+
  aes(color=series,linetype=series)+
  scale_color_manual(values=c('black','blue'))+
  scale_linetype_manual(values=c(1,2)) 

accuracy(fore,Esales)
