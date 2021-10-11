# removes all variables to clear out the environment 
remove(list=ls())

# to know current directory
getwd()

# used to set working directory. Note that it wont create a folder if missing
setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting')

# list contents of current directory
dir()

# lists current variables that we are working with
ls()

x = 4
y = 7
x+y

# if we wish to create an array of numbers, we can use the combine function
cowboys = c(10,9,8)
cowboys

# install.packages('forecast') -> to install forecast package
install.packages('forecast')

# mention below for using a particular package which is installed
library(forecast)

setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/1 - Jan 20')

# to read a csv file into a dataframe, we use read.csv
x = read.csv('day1.csv')
x

# to check the attributes of a variable 
attributes(x)

# lists the dimensions of a variable - like rows and columns
dim(x)

# for accessing data in R row and C columns, we use x[R,C]
# below will give data from 1st row and 2nd column of x
x[1,2]

# to get first 4 values in 2nd column
x[1:4,2]

# to get 2nd row of x
x[2,]

# create a timeseries 
sw=ts(x[,2],start=c(1993,1),end=c(2020,3),frequency=4)

# class is ts
attributes(sw)

# above ts can also be created using below
#Can use $ sign to reference the dataframe
sw=ts(x$sw,start=c(1993,1),end=c(2020,3),frequency=4)

# for plotting the data
plot(sw)
sw

# hold out the last 3 records and not use them in forecasting
swhold=ts(x[,2],start=c(1993,1),end=c(2019,4),frequency=4)
swhold

# ffp2 is forecasting principles and packages by Rob Hyndman
install.packages('fpp2')

# load the library
library(fpp2)

# sample dataset loaded from fpp2 -> air passengers data
AirPassengers

# window allows to view only certain portion of data
a = window(sw,start=c(2019,1))
a

# will create a timeseries
attributes(a)

# using naive forecast, will forecast into next 3 periods
nf=naive(swhold,h=3)
nf

# class is forecast
attributes(nf)

mf=meanf(swhold,h=3)
mf
attributes(mf)

# Exponential smoothing methods characterizes the data into 3 parts
# a) Level (random)
# b) Trend
# c) Seasonal 

# decompose can be used to separate the parts
plot(decompose(swhold))

# for Simple Exponential Smoothing (where trend and seasonality is zero): 
SESsw=ses(swhold,h=3)
SESsw

# will list all the options for that object - just like attributes
names(SESsw)

# will list options and class
attributes(SESsw)

# use summary to know the details of the object
summary(SESsw)

plot(SESsw)

# SES with trend is Holts method
# Use Additive method if data has increasing positive trend
# by default its additive trend
# after a while, the forecast increases at a constant trend value

HoltA=holt(swhold,h=3)
HoltA
plot(HoltA)

# If after a certain period, if trend needs to be slowed down after a period
# then use Additive damping method

HoltAd=holt(swhold,h=3,damped=TRUE)
HoltAd

# Multiplicative trend - R doesn't allow to estimate this model
HoltM=holt(swhold,h=3,exponential=TRUE)

# Multiplicative trend with damping 
HoltMd=holt(swhold,h=3,exponential=TRUE,damped=TRUE)
HoltMd

install.packages('ggplot2')
library(ggplot2)

# autoplot is from ggplot2
# we can also add labels in below manner
autoplot(swhold)+labs(title='SW revenue since 1993',x='year',y="revenue")

# below is using autoplot from forecast package
forecast::autoplot(swhold)+labs(y="revenue")

# used to plot data by frequency
monthplot(swhold)

# shows a seasonal plot by frequency
ggseasonplot(swhold)

seasonplot(swhold)

# For data that exhibits potential non-linear trend, we often can consider a Box-Cox
# transformation. The graph is not depicted, but it reveals a much more linear looking series.
# We will review the Box-Cox transformation in the coming weeks. The logarithmic transformation 
# is a special case of a Box-Cox transformation.

autoplot(log(swhold))+labs(y="revenue")

# To capture seasonality, we use Holt-Winters method: 

# for additive seasonality (default)
swHWadditive=hw(swhold,h=3)
summary(swHWadditive)

# for multiplicative seasonality
swHWmultiplicative=hw(swhold,h=3,seasonal='multiplicative')
summary(swHWmultiplicative)

# to calculate accuracy statistics, we need forecast object and out of sample data
# to compare our forecast to 

# compares with data in training
accuracy(swHWadditive)

# to compare with out of sample data as well
# here swHWadditive is forecast object and sw is time series
sw
accuracy(swHWadditive,sw)
accuracy(swHWmultiplicative,sw)

# ets model estimates on its own and gives a exponential smoothing model: 
ETSmodel=ets(swhold)
attributes(ETSmodel)
ETSmodel  

# to actually forecast, we use forecast function that looks for a model
fit=forecast(ETSmodel, h=3)
fit
attributes(fit)
accuracy(ETSmodel)

# below wont work as ETSmodel is not forecast object but a model
accuracy(ETSmodel,sw)

# use forecast and ts objects while using accuracy when comparing training and test
accuracy(fit,sw)

sw
# learn plotting different plotting methods for graphs
revenue=window(sw,start=c(2018,1))
forecast=fit$mean
upper95=fit$upper[,2]
lower95=fit$lower[,2]

graph=ts.union(revenue,forecast,upper95,lower95)

# default name will be 'series' when plotted
autoplot(graph)

# change aesthetics using aes
autoplot(graph)+
  aes(color=series,linetype=series)+
  scale_color_manual(values=c('black','blue','red','red'))+
  scale_linetype_manual(values=c(1,2,3,3))+
  labs(x='Years',y='Revenue')

# for installing tidyverse package
install.packages('tidyverse')
library(tidyverse)

install.packages('tsibble')
library(tsibble)

#when using a tsibble or tibble, we need to use correct data format(yyyy-mm-dd)

# below will create a tibble
temp=read_csv('day1.csv')
temp
attributes(temp)

# a timeseries tibble is tsibble
# 2 most important things in tsibble are 'keys' and time index

# R allows to pass the output of one function into another using pipes %>%
# change the date so that R knows its looking at quarterly data using function 
# called mutate

# to convert a tibble into tsibble, we need to let R know what is time index

swData=temp%>%mutate(quarter=yearquarter(Date))%>%select(-Date)%>%as_tsibble(index=quarter) 
# same as below:
#swData=mutate(temp,quarter=yearquarter(Date))

swData
# to know about any function, use ? like below: 
?yearquarter

# will select all columns except Date
#select(-Date)

# cntl + shift + m is the shortcut for pipe operator %>% 

install.packages('feasts')
library(feasts)

install.packages('dplyr')
library(dplyr)

# plot southwest data against lags of itself (using ggplot)
# sw is the column name within the tsibble swData

# shows 9 lags at a time
gg_lag(swData,sw,geom='point')

# shows 1 thru 4 lags
gg_lag(swData,sw,geom='point',lags=1:4)

# shows 4 lag
gg_lag(swData,sw,geom='point',lags=4)

# perform a regression of sw revenue in one quarter on revenue from one year ago
# REVt on REVt-4

# i.e use data from 1994 Q1 thru 2020 Q3 and 
# 1993 Q1 thru 2019 Q3

lag0=filter_index(swData,'1994 Q1'~'2020 Q3')
lag4=filter_index(swData,'1993 Q1'~'2019 Q3')

lag0
lag4

# a linear regression model can be fit using lm
model=lm(lag0$sw~lag4$sw)
summary(model)
model

# use below for including multiple variables (like u in below case). Here + acts as AND operator
#model=lm(lag0$sw~lag4$sw+u)

# look up the residuals and obtain the sum of squares of residuals 
plot(model$residuals)
ssr=sum(model$residuals^2)
ssr

# total sum of squares is square of (data-mean) values
tss=sum((lag0$sw-mean(lag0$sw))^2)
tss

# calculate the R-Squared
r2=1-(ssr/tss)
r2


x=rnorm(107)
model1=lm(lag0$sw~lag4$sw+x)
summary(model1)

# by adding random variable x, R2 improved. Hence R2 is not a viable parameter 
# for model selection. It will just improve by adding more variables. 

e=rnorm(5000)
ggtsdisplay(e,lag.max=30)

# always stores as an array, and first element will contain the actual value
y=0
y[1]

a = logLik(model)
b = logLik(model1)
a
b

AIC(model)
BIC(model)
AIC(model1)
BIC(model1)

# Load necessary libraries
library(forecast)
library(ggplot2)

# get the working directory
getwd()

# set the working directory to where the data is present
setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting/Exam 1')

# read and load the file
temp=read.csv('grocery.csv')

# create time series for entire data
grocery=ts(temp$Store,start=c(1992,1),end=c(2020,12),frequency=12)

# Hold out the records for out of sample, to be used for testing the model accuracy
groceryHOLD=ts(temp$Store,start=c(1992,1),end=c(2020,6),frequency=12)

ETSmodel=ets(groceryHOLD)
fit=forecast(ETSmodel,h=6)
fit

# Measure accuracy for the model
accuracy(fit,grocery)

#grocery_temp=ts(temp$Store,start=c(2020,7),end=c(2020,12),frequency=12)
#accuracy(fit,grocery_temp)
#attributes(grocery_temp)
#fit

grocery_temp1=window(grocery, start=c(2020,7))
grocery_temp1
attributes(grocery_temp1)
accuracy(fit,grocery_temp1)

#grocery_temp2=ts(grocery,start=c(2020,7),end=c(2020,12),frequency=12)
#accuracy(fit,grocery_temp2)
#attributes(grocery_temp2)

grocery_temp3=ts(temp$Store[343:348],start=c(2020,7),end=c(2020,12),frequency=12)
accuracy(fit,grocery_temp3)

model=ets(groceryHOLD,lambda=BoxCox.lambda(groceryHOLD))
fore=forecast(model,h=6,lambda=BoxCox.lambda(groceryHOLD),biasadj=TRUE)

accuracy(fore,grocery)

# create the separate ts for plotting actual, forecast, upper 
# and lower 80% confidence bands
actual=window(grocery,start=c(2020,1))
forecast=fore$mean
upper80=fore$upper[,1]
lower80=fore$lower[,1]

# combine the 4 individual time series into one object
graphplot=ts.union(actual,forecast,upper80,lower80)

# PLot the graph and change aesthetics
autoplot(graphplot)+
  aes(color=series,linetype=series)+
  scale_color_manual(values=c('black','blue','red','red'))+
  scale_linetype_manual(values=c(1,2,3,3))+
  labs(x='Period',y='Sales',title='Store Sales -> Actual vs Forecast')

fore
upper80
lower80
forecast
