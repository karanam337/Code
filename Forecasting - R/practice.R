#install.packages('forecast')
getwd()
setwd('C:/MSBA/Spring 2021/ECON 5337 - Business & Economic Forecasting')
dim(file)
file[1,3]
dir()
library(forecast)

# to remove all variables from the session
remove(list=ls())

install.packages('fpp2')
library(fpp2)

# dataset
AirPassengers

# below creates a dataframe
temp = read.csv('day1.csv')
temp

attributes(temp)

# below creates a tibble, we use read_csv 
swHOLD = ts(temp[,2], start=c(1993,1), end=c(2019,4), frequency=4)
plot(swHOLD)

sw = ts(temp[,2], start=c(1993,1), frequency=4)
plot(sw)

install.packages('ggplot2')
library(ggplot2)
