# ECON 5337 - Business & Economic Forecasting 
# Project Work - Group# 2 

# Team members: 
# 1) Phanikrishna Karanam
# 2) Joel Andrews
# 3) Swetha Gollamudi
# 4) Hari Priya Jujjavarapu
# 5) Amruta Joshi

# Dataset: Single family houses sold in USA
# Source: https://fred.stlouisfed.org/series/HSN1FNSA

# Dataset is about number of new single family houses sold in USA. Data ranges from Jan 1963 to Feb 2021.
# We chose the dataset on number of new homes sold, instead of actual home price as we feel often when
# sales are low, builders chose not to reduce the list price of the home for driving a sale. Instead they 
# either increase the list price or have it at the same level but offer discounts, incentives, 
# free upgrades, zero closing costs and other benefits which in a way do not reveal the actual sale price 
# of the home. Hence the number of new family homes would serve as a better indicator on performance of 
# housing market and the economic conditions.

# We think house sales might be impacted due to Covid pandemic. Let us forecast and compare with actual
# data on how they compare.

# Clear the environment
remove(list=ls())

# Load necessary libraries
library(forecast)
library(ggplot2)
library(urca)

# Set the working directory to the location of the input file
setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/Project')

# Read the input file
temp=read.csv('HSN1FNSA.csv')
temp

# Create a time series of entire data with monthly frequency
hsold=ts(temp[,2],start=c(1963,1),frequency=12)
hsold

# Create a time series excluding the last 4 observations for model training
hsoldHOLD=ts(temp[,2],start=c(1963,1),end=c(2020,10),frequency=12)
hsoldHOLD

# The plot of data reveals high volatility and non linear nature. 
# No trend observed initially but seems to be picking up after late 90's and early 2010's onwards.
# The drastic drop observed during 2007 was due to the financial crisis created by housing market.
autoplot(hsoldHOLD)

# Lambda=0.20 is far from 1, indicating transformation is going to be likely useful
# Also lambda is somewhat far from zero, indicating Box Cox transformation to be more useful 
# over simple logarithmic transformation
hsoldLAMBDA=BoxCox.lambda(hsoldHOLD)
hsoldLAMBDA

# Transform data series using Box Cox transformation
transform=BoxCox(hsoldHOLD,lambda=hsoldLAMBDA)

# Data still appears to be non linear and volatility is also present
# It seems Box Cox transformation is not helping with this series. We will use original data
autoplot(transform)

# Let us perform a unit root test to check for covariance stationarity of original data
# Since data includes trend, let us include trend as part of ADR test
summary(ur.df(hsoldHOLD,type='trend',lags=37,selectlags='AIC'))

# The test statistic of -3.051 exceeds all the critical values (tau3)
# Hence we fail to reject a unit root in the data series
# Note: NULL that says a unit root is present indicating data is not covariance stationary

# Below indicates presence of a seasonal unit root in the data series
nsdiffs(hsoldHOLD)

# The presence of seasonal unit root could have influenced the first ADR test
# Let us seasonally difference the data and perform the ADR test again
autoplot(diff(hsoldHOLD,12))

# The plot of seasonally differenced data shows that trend has likely been eliminated
# Let us run the ADR test using drift
summary(ur.df(diff(hsoldHOLD,12),type='drift',lags=37,selectlags='AIC'))

# The test statistic -3.9416 is less than all of the critical values thus allowing
# us to reject the NULL and indicating absence of non seasonal root in the data series.  
# Hence we conclude only seasonal differencing is required for data to be covariance stationary 

# The initial plot of data shows no trend during early years. It gradually picked up during late 90's 
# and drastically drops during the 2007 financial crisis. It again starts to pick up from 2011 onwards.
# Plot of decompose function confirms the same & separates Level (random), trend & seasonality in data
autoplot(decompose(hsoldHOLD))

# Let us explore few other seasonal plots by frequency
monthplot(hsoldHOLD)
seasonplot(hsoldHOLD)
ggseasonplot(hsoldHOLD)

# It appears that home sales pick up during the 1 quarter of the year, remain steady during 2nd quarter 
# and slow down & fall towards the last 2 quarters. This must be related to more construction / sales 
# happening during the Spring & Summer months. Construction / Sales slow down during Fall & Winter months,
# due to snow fall in the Northern, North Eastern and North Western parts of USA. 

# We will use SARIMA methods to model the data
# We already concluded d=0 and D=1 from the earlier ADF test and nsdiffs
# Let us review the ggtsdisplay of the seasonally differenced data with 37 as maximum lags
# i.e upto 3+ years of data to account for any seasonality factors arising out of any events

ggtsdisplay(diff(hsoldHOLD,12),lag.max=37)

# From the ACF plot:
# - Smooth exponential decay observed upto 12 lags, however beyond that decay is not consistent with 
#   no visible patterns. Also ACF is statistically significant at several lags, indicating presence 
#   of AR component
# - There are statistically significant coefficients at seasonal lags 12, 24(somewhat), 36 and also 
#   around those seasonal lags with complex decay patterns, indicating presence of seasonal AR.
# - Not much can be inferred regarding the moving average piece from the plot

# From the PACF plot: 
# - The first coefficient is large and the second one also appears to be statistically significant. 
# - PACF is statistically significant at very few non seasonal lags, and has statistically significant 
#   coefficients at seasonal lags. It's possible a MA component exists, we could start q=0 onwards 
# - Statistically significant coefficients observed at seasonal lags, with amplified behavior at lag 12. 
#   Around the seasonal lags, statistically significant coefficients are observed indicating interaction 
#   between seasonal and non seasonal pieces of MA components. The decay at seasonal lags 12, 24, 36 
#   appears to be smooth. However, we could start with Q=0 onwards as this behavior could be due to 
#   presence of seasonal AR piece as well 

# To summarize: 
# - The ACF shows smooth decay up to 12 lags and there after not consistent. The PACF has 1 large 
#   coefficient and a second one that is statistically significant. We can start with p=2 onwards
# - Considering statistically significant coefficients for seasonal lags in ACF and amplified behavior at 
#   PACF lag 12 alone, we start P=1 onwards.   

# Let us estimate SARIMA(p,d,q)x(P,D,Q) models as per below: 
#	p=2 onwards, P=1 onwards
#	d=0 and D=1 (only seasonally differencing)
#	q=0 onwards, Q=0 onwards

#SARIMA(2,0,0)x(1,1,0)
#SARIMA(2,0,1)x(1,1,0)
#SARIMA(2,0,1)x(1,1,1) 
#SARIMA(2,0,0)x(2,1,0)
#SARIMA(2,0,1)x(2,1,0)
#SARIMA(2,0,1)x(2,1,1)

Arima(hsoldHOLD,order=c(2,0,0),seasonal=c(1,1,0))
Arima(hsoldHOLD,order=c(2,0,1),seasonal=c(1,1,0))
Arima(hsoldHOLD,order=c(2,0,1),seasonal=c(1,1,1))
Arima(hsoldHOLD,order=c(2,0,0),seasonal=c(2,1,0))
Arima(hsoldHOLD,order=c(2,0,1),seasonal=c(2,1,0))
Arima(hsoldHOLD,order=c(2,0,1),seasonal=c(2,1,1))

# #	        Model	          AIC	    BIC
# 1	SARIMA(2,0,0)x(1,1,0)	4177.75	4195.85
# 2	SARIMA(2,0,1)x(1,1,0)	4172.51	4195.14
# 3	SARIMA(2,0,1)x(1,1,1)	4028.72	4055.87  -> 1st best (lowest AIC and BIC)
# 4	SARIMA(2,0,0)x(2,1,0)	4134.88	4157.5
# 5	SARIMA(2,0,1)x(2,1,0)	4129.57	4156.72
# 6	SARIMA(2,0,1)x(2,1,1)	4029.86	4061.53  -> 2nd best

# SARIMA(2,0,1)x(1,1,1) model has the lowest AIC and BIC among all estimated models
# It may be the best fit model, let us save that as our model 
model_sarima=Arima(hsoldHOLD,order=c(2,0,1),seasonal=c(1,1,1))
model_sarima

# Analyze the residuals using ggtsdisplay for selected model
ggtsdisplay(model_sarima$residuals,lag.max=37)

# With almost all ACF & PACF coefficients being statistically insignificant (within 95% confidence bands
# except for lag 20), we see there is small evidence of residual serial correlation.
# It is expected to have up to 5% of coefficients to be outside the confidence bands and also considering 
# this lag being at position 20 and slightly significant, we can conclude that the residuals lack 
# serial correlation and properties are consistent with white noise. Hence estimated model seems like a 
# good fit for us to proceed

# Let us also perform a diagnostic test using checkresiduals to see if our residuals
# display any characteristics that are inconsistent with white noise. 
checkresiduals(model_sarima)

# As before, we see all ACF coefficients are statistically insignificant (except for lag 20).
# The Ljung-Box test also provides significant p-value of 0.16 in excess of conventional test sizes.
# Thus model is a reasonable fit for us to work with.

# Using the selected model, we forecast 4-step ahead (Nov 2020 thru Feb 2021)
fore_sarima=forecast(model_sarima,h=4)
fore_sarima

# Forecasted values for horizon of 4 and actual data from Jan 2020
forecast_sarima=fore_sarima$mean
actual_hsold=window(hsold,start=c(2020,1))
upper95_sarima=fore_sarima$upper[,2]
lower95_sarima=fore_sarima$lower[,2]

# Generate plot of forecasted values along with actual values
graph_sarima=ts.union(actual_hsold,forecast_sarima,upper95_sarima,lower95_sarima)
autoplot(graph_sarima,xlab='Years',ylab='Houses Sold (in thousands)',main='Forecast vs Actual - SARIMA')+
  aes(color=series,linetype=series)+
  scale_color_manual(values=c('black','blue','red','red'))+
  scale_linetype_manual(values=c(1,2,3,3)) 

# Our forecasts are almost within the 95% confidence bands and the housing market is growing. It seems to 
# have recovered well after the significant crash during the 2007 financial crisis. Also Covid pandemic 
# does not seem to have slowed down the housing market as expected initially and also the fact that 
# housing interest rates are at historic low could also be a factor that is driving the sales. 
# This is a good sign for construction industry and US economy.

# Let us estimate an ETS model 
model_ETS=ets(hsoldHOLD)
model_ETS

# ETS function chose Multiplicative forecast errors, None for trend, Multiplicative seasonality for data
# We were expecting to see Additive trend or Additive damped trend, however looks like trend is modeled 
# as None on an overall basis considering the entire duration of timeseries. 

# Let us use ETS model to forecast 4-step ahead (Nov 2020 thru Feb 2021): 
fore_ETS=forecast(model_ETS,h=4)
fore_ETS

# Forecasted values for horizon of 4 and actual data from Jan 2020
forecast_ETS=fore_ETS$mean
upper95_ETS=fore_ETS$upper[,2]
lower95_ETS=fore_ETS$lower[,2]

# Generate plot of forecasted values along with actual values
graph_ETS=ts.union(actual_hsold,forecast_ETS,upper95_ETS,lower95_ETS)
autoplot(graph_ETS,xlab='Years',ylab='Houses Sold (in thousands)',main='Forecast vs Actual - ETS')+
  aes(color=series,linetype=series)+
  scale_color_manual(values=c('black','blue','red','red'))+
  scale_linetype_manual(values=c(1,2,3,3)) 

# Our forecasts are well within the 95% confidence bands and this model seems to be performing better  
# than the previous SARIMA model. 

# Let us inspect the accuracies of both SARIMA and ETS models
accuracy(fore_sarima,hsold)
accuracy(fore_ETS,hsold)

# Looking at MAE and RMSE statistics for both the models, 
# ETS is a slightly better model than SARIMA. We choose ETS as the best fit model for forecasting
#
# MAE:
#      -> 7.50 (ETS TEST)  < 7.60 (SARIMA TEST)
#      -> 3.40 (ETS Train) < 3.44 (SARIMA TEST)
# RMSE:
#      -> 8.92 (ETS TEST)  < 9.19 (SARIMA TEST)
#      -> 4.38 (ETS Train) < 4.48 (SARIMA TEST)

# Let us estimate the entire sample using the selected ETS model
model=ets(hsold)

# Let us forecast 6 steps ahead (Mar 2021 thru Aug 2021)
fore_model=forecast(model,h=6)
fore_model

# Forecasted values for horizon of 6 and actual data from Jan 2020
forecast_model=fore_model$mean
upper95_model=fore_model$upper[,2]
lower95_model=fore_model$lower[,2]

# Generate plot of forecasted values along with actual values
graph_model=ts.union(actual_hsold,forecast_model,upper95_model,lower95_model)
autoplot(graph_model,xlab='Years',ylab='Houses Sold (in thousands)',main='Forecast vs Actual')+
  aes(color=series,linetype=series)+
  scale_color_manual(values=c('black','blue','red','red'))+
  scale_linetype_manual(values=c(1,2,3,3)) 

# The forecasted values are consistent with the seasonality noted in sales where they tend to taper off
# a bit towards the 3rd and 4th quarters of the year. As mentioned earlier, a plausible explanation could 
# that fewer Construction / Sales during Fall & Winter months, due to snow fall in the Northern, 
# North Eastern and North Western parts of USA. 

# These forecasts would provide an understanding of the housing market for the next half year.
# This provides insights on where the construction industry and US economy are heading. 