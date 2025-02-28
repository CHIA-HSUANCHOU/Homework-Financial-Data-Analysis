##Script for Exercise 3
##Fill "____" to complete the codes
##But don't forget to answer questions

## 1.
## simulate a random walk
## a. set S_0 = 3.5
s_0<-3.5

## Generate 200 Z~i.i.d.N(0,1) with rnorm
z<-rnorm(n = 200, mean = 0, sd = 1)

## Put S_0 and Z together
rw<-c(s_0, z)

## Use cumsum to generate the path and plot it 
s_t<-cumsum(rw)
plot(s_t, type = "l")

-------------------------------------------------------------------------------------
## b.Repeat the simulation in a. 5000 times to get 5000 simulated S_200
## set S_0 = 3.5
s_0<-3.5

## Generate a numeric vector with length 5000 
## for storing the generated S_200
s_200<-numeric(5000)  

## Use a "for" loop to obtain S_200
for(i in 1:5000){
  
  z<-rnorm(n = 200, mean = 0, sd = 1)
  s_200[i]<-s_0+sum(z)
  
}

## Calculate mean and variance of the generated s_200
mean(s_200)
var(s_200)
------------------------------------------------------------------------------------------
## 2. Calculate daily returns
## Import the data
## Remember to deal with the missing values
## by setting na.strings appropriately
rm(list = ls(all =TRUE))
data_2330<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW3/2330.TW.csv", sep = ",", na.strings = "null", header = T)
data_3008<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW3/3008.TW.csv", sep = ",", na.strings = "null", header = T)

## Transform Date with as.Date
data_2330[,1]<-as.Date(data_2330[,1])
data_3008[,1]<-as.Date(data_3008[,1])
## -------------------------------------------------------------------------------------
## a.Check where NA's of adj.Close happen
na_2330<-which(is.na(data_2330[,6]))
na_3008<-which(is.na(data_3008[,6]))
na_2330
na_3008

## Replace the missing adj.Close with previous day's value
## Use "for" loop for 2330
for(i in na_2330){
  
  data_2330[i, 6]<-data_2330[(i-1), 6]
  
}
which(is.na(data_2330[, 6]))

## Use a "for" loop for 3008
for(i in na_3008){
  
  data_3008[i, 6]<-data_3008[(i-1), 6]
  
}
which(is.na(data_3008[, 6]))

## NA's of the adj.close price should be all replaced
## --------------------------------------------------------------------------
## b. load my_function.R
source("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW3/my_function.R")  

## -----------------------------------------------------------------------
## c.Calculate returns
## need to load functions retx and logrx for 
## calculating simple returns and log returns

## 2330, simple returns and log returns
data_2330$ret<-c(NA, retx(data_2330$Adj.Close))
data_2330$lret<-c(NA, logrx(data_2330$Adj.Close))

## 3008, simple returns and log returns
data_3008$ret<-c(NA, retx(data_3008$Adj.Close))
data_3008$lret<-c(NA, logrx(data_3008$Adj.Close))

## Summary statistics, standard deviation, skewness and kurtosis
## need to load functions my_skewness and my_kurtosis for calculating skewness and kurtosis 
## note that NA (the first entry) should be removed when calculating std, skewness and kurtosis

## 2330
c(summary(data_2330$ret), sd(data_2330$ret[-1]), 
  my_skewness(data_2330$ret[-1]), my_kurtosis(data_2330$ret[-1]))
c(summary(data_2330$lret), sd(data_2330$lret[-1]), 
  my_skewness(data_2330$lret[-1]), my_kurtosis(data_2330$lret[-1]))

## 3008
c(summary(data_3008$ret), sd(data_3008$ret[-1]), 
  my_skewness(data_3008$ret[-1]), my_kurtosis(data_3008$ret[-1]))
c(summary(data_3008$lret), sd(data_3008$lret[-1]), 
  my_skewness(data_3008$lret[-1]), my_kurtosis(data_3008$lret[-1]))

## ----------------------------------------------------------------
## d.calculate gross (cumulative) returns
## with simple return, use "cumprod"
data_2330$cum.ret<-c(NA, cumprod((1+data_2330$ret[-1])))
data_3008$cum.ret<-c(NA, cumprod((1+data_3008$ret[-1])))

## with log return, use "cumsum"
data_2330$cum.lret<-c(NA, cumsum(data_2330$lret[-1]))+1
data_3008$cum.lret<-c(NA, cumsum(data_3008$lret[-1]))+1

## -----------------------------------------------------------------------
## e. Calculate weekly and monthly returns
## Load library xts
library(xts)

## Follow the procedures used in slides
## Combine data, open, high, low, close, volume and adj.Close 
## They have the same number of observations!
data_2330<-cbind(data_2330[, 1:5], data_2330[, 7:6])
data_3008<-cbind(data_3008[, 1:5], data_3008[, 7:6])
colnames(data_2330)[6:7]<-c("Volume", "Adjusted")
colnames(data_3008)[6:7]<-c("Volume", "Adjusted")

## Calculate weekly return
## 2330
data_2330w<-to.weekly(data_2330, indexAt = "firstof")
data_2330w$ret<-c(NA, retx(data_2330w[,6]))
data_2330w$lret<-c(NA, logrx(data_2330w[,6]))
head(data_2330w)

## 3008
data_3008w<-to.weekly(data_3008, indexAt = "firstof")
data_3008w$ret<-c(NA, retx(data_3008w[,6]))
data_3008w$lret<-c(NA,logrx(data_3008w[,6]))
head(data_3008w)

## Calculate monthly return 
## 2330
data_2330m<-to.monthly(data_2330, indexAt = "firstof")
data_2330m$ret<-c(NA, retx(data_2330m[,6]))
data_2330m$lret<-c(NA, logrx(data_2330m[,6]))
head(data_2330m)

## 3008
data_3008m<-to.monthly(data_3008, indexAt = "firstof")
data_3008m$ret<-c(NA, retx(data_3008m[,6]))
data_3008m$lret<-c(NA,logrx(data_3008m[,6]))
head(data_3008m)

## ---------------------------------------------------------------------
## f. plot histogram
## Note that here we need to recalculate "daily" simple 
## and log returns in data_3008
data_3008$ret<-c(NA, retx(data_3008$Adjusted))
data_3008$lret<-c(NA, logrx(data_3008$Adjusted))

## Set a 2 x 3 frame
windows(width=10, height=8)
par(mfrow=c(2,3))

## Simple return: daily, weekly and monthly
hist(data_3008$ret); hist(data_3008w$ret); hist(data_3008m$ret)

## Log return: daily, weekly and monthly
hist(data_3008$lret); hist(data_3008w$lret); hist(data_3008m$lret)
## --------------------------------------------------------------------------------
## g. Plot acf
## Note that here we need to recalculate "daily" simple 
## and log returns in data_2330
data_2330$ret<-c(NA, retx(data_2330$Adjusted))
data_2330$lret<-c(NA, logrx(data_2330$Adjusted)) 

windows(width=10, height=8)
## Set 2 x 3 frame  
par(mfrow = c(2,3))

## Simple return: daily, weekly and monthly
acf(data_2330$ret, na.action = na.pass)
acf(data_2330w$ret, na.action = na.pass) 
acf(data_2330m$ret, na.action = na.pass)


## Log returns: daily, weekly and monthly
acf(data_2330$lret, na.action = na.pass)
acf(data_2330w$lret, na.action = na.pass) 
acf(data_2330m$lret, na.action = na.pass)

## ------------------------------------------------------------------------------
## h.modify acf1, call the modified function as acf1x
my_acf1x<-function(x, na.action = na.fail){
  
  acf(x, plot = F, na.action = na.action)[[1]][2] 
  
}


## Calculate acf(1) with acf1x, simple returns: daily, weekly and monthly
my_acf1x(data_2330$ret, na.action = na.pass)
my_acf1x(data_2330w$ret, na.action = na.pass)
my_acf1x(data_2330m$ret, na.action = na.pass)

## Calculate acf(1) with acf1x, log returns: daily, weekly and monthly
my_acf1x(data_2330$lret, na.action = na.pass)
my_acf1x(data_2330w$lret, na.action = na.pass)
my_acf1x(data_2330m$lret, na.action = na.pass)