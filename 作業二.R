## Script for Exercise 2
## Fill "___" to complete the codes
## But don't forget to answer questions
rm(list = ls (all = TRUE))
##1. import data
data_2317<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/2317.TW.csv", sep=",", header = T)
data_2330<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/2330.TW.csv", sep=",", header = T)
data_2412<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/2412.TW.csv", sep=",", header = T)
data_2882<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/2882.TW.csv", sep=",", header = T)
data_3008<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/3008.TW.csv", sep=",", header = T)

## ---------------------------------------------------------------------------------------------

## 2. manually check the imported data 
## Use function "class" on some columns of the imported data 
## For example, choose the second column  
class(data_2317[,2])

## Try another imported data, say 2882 and the fourth column
class(data_2882[,4])

## --------------------------------------------------------------------------------------------
## 3. Check missing values
## Show some data
data_2317[1:3,2]             

## Try another imported data, say 2882, 
## 10 to 15 rows and the fourth columns
data_2882[10:15,4] 


## Check how many missing data in these columns 
sum(data_2882[,2]=="null")  
## Check how many missing data in these data 
sum(data_2317[,2:7]=="null")
sum(data_2330[,2:7]=="null")
sum(data_2412[,2:7]=="null")
sum(data_2882[,2:7]=="null")
sum(data_3008[,2:7]=="null")
## ------------------------------------------------------------------------------------------

## 4. Re-imported the data and set na.strings appropriately 
rm(list = ls (all = TRUE))

## Set na.strings = "null" for the missing value
data_2317<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/2317.TW.csv", sep=",", na.strings = "null", header = T)
data_2330<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/2330.TW.csv", sep=",", na.strings = "null", header = T)
data_2412<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/2412.TW.csv", sep=",", na.strings = "null", header = T)
data_2882<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/2882.TW.csv", sep=",", na.strings = "null", header = T)
data_3008<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW2/3008.TW.csv", sep=",", na.strings = "null", header = T)

## Check class of one of the open, high, low, close again, for example
class(data_2317[,2])
class(data_2317[,3])
class(data_2317[,4])
class(data_2317[,5])
class(data_2317[,6])
class(data_2317[,7])
## Try another imported data, say 2882 and the fourth column
class(data_2882[,4])

## -----------------------------------------------------------------------------------------

## 5. Before calculating summary statistics, remember to deal with "Date"
data_2317[,1]<-as.Date(data_2317[,1])
data_2330[,1]<-as.Date(data_2330[,1])
data_2412[,1]<-as.Date(data_2412[,1])
data_2882[,1]<-as.Date(data_2882[,1])
data_3008[,1]<-as.Date(data_3008[,1])

## Clean the data
## Raw data's dimension
dim(data_2317);dim(data_2330);dim(data_2412);dim(data_2882);dim(data_3008)

## Summary statistics for the five stocks
summary(data_2317)
summary(data_2330)
summary(data_2412)
summary(data_2882)
summary(data_3008)

##-----------------------------------------------------------------------------------------

## 6. check NA's for the open price, for example
which(is.na(data_2317[, 2])) 
which(is.na(data_2317[, 3])) 
which(is.na(data_2317[, 4])) 
which(is.na(data_2317[, 5])) 
which(is.na(data_2317[, 6])) 
which(is.na(data_2317[, 7])) 
na_2317<-which(is.na(data_2317[, 2]))            ## Construct an index for NA observations
data_2317[na_2317, 1]                            ## dates for the NA observations
weekdays(data_2317[na_2317, 1])                  ## weekdays for the NA observations


## Try another one (2330)
which(is.na(data_2330[, 2])) 
which(is.na(data_2330[, 3])) 
which(is.na(data_2330[, 4])) 
which(is.na(data_2330[, 5])) 
which(is.na(data_2330[, 6])) 
which(is.na(data_2330[, 7])) 
na_2330<-which(is.na(data_2330[, 2]))   
data_2330[na_2330, 1]
weekdays(data_2330[na_2330, 1])

## Try the rest stocks by yourself
## 2412 
which(is.na(data_2412[, 2])) 
which(is.na(data_2412[, 3])) 
which(is.na(data_2412[, 4])) 
which(is.na(data_2412[, 5])) 
which(is.na(data_2412[, 6])) 
which(is.na(data_2412[, 7])) 

na_2412<-which(is.na(data_2412[, 2]))   
data_2412[na_2412, 1]
weekdays(data_2412[na_2412, 1])

## 2882
which(is.na(data_2882[, 2])) 
which(is.na(data_2882[, 3])) 
which(is.na(data_2882[, 4])) 
which(is.na(data_2882[, 5])) 
which(is.na(data_2882[, 6])) 
which(is.na(data_2882[, 7])) 

na_2882<-which(is.na(data_2882[, 2]))   
data_2882[na_2882, 1]
weekdays(data_2882[na_2882, 1])
## 3008
which(is.na(data_3008[, 2])) 
which(is.na(data_3008[, 3])) 
which(is.na(data_3008[, 4])) 
which(is.na(data_3008[, 5])) 
which(is.na(data_3008[, 6])) 
which(is.na(data_3008[, 7])) 

na_3008<-which(is.na(data_3008[, 2]))   
data_3008[na_3008, 1]
weekdays(data_3008[na_3008, 1])
##--------------------------------------------------------------------------

##7. Eliminate NA's with complete.cases
data_2317<-data_2317[complete.cases(data_2317), ]
data_2330<-data_2330[complete.cases(data_2330), ]
data_2412<-data_2412[complete.cases(data_2412), ]
data_2882<-data_2882[complete.cases(data_2882), ]
data_3008<-data_3008[complete.cases(data_3008), ]

##-------------------------------------------------------------------------

## 8. plot time-series plots of 2317 and 2330
## two rows and two columns
windows(height = 8, width = 10)  ## or quartz if Mac OS is used
par(mfrow=c(2,2))

##plots
plot(x = data_2317$Date, y = data_2317$Adj.Close, type = "l",
     xlab = "Date", ylab = "Adjusted Close Price", main = "Adjusted Close Price, 2317")
plot(x = data_2330$Date, y = data_2330$Adj.Close, type = "l",
     xlab = "Date", ylab = "Adjusted Close Price", main = "Adjusted Close Price, 2330")
plot(x = data_2317$Date, y = data_2317$Volume, type = "h",
     xlab = "Date", ylab = "volume", main = "Volume, 2317")
plot(x = data_2330$Date, y = data_2330$Volume, type = "h",
     xlab = "Date", ylab = "volume", main = "Volume, 2330")

## -------------------------------------------------------------------------

## 9. plot normalized prices with close and adjusted close prices of 2412
data_2412_np<-data_2412$Close/data_2412$Close[1]                ##with close price
data_2412_np1<-data_2412$Adj.Close/data_2412$Adj.Close[1]                 ##with adjusted price

windows(height = 8, width = 10)        ## or quartz if Mac OS is used
par(mfrow=c(2,1))

## Set range
rangex<-range(c(data_2412_np, data_2412_np1), na.rm = T)

plot(x = data_2412$Date, y = data_2412_np, ylim = rangex, 
     type = "l", xlab = "Date", ylab = "Value of Investment ($)", 
     col = "black", lty = 1, lwd = 2, cex.lab = 1.5, cex.axis = 1.2,
     main = "Value of $1 Investment in 2412, Dec-31-2013 ~ Dec-30-2022")
lines(x = data_2412$Date, y = data_2412_np1,
      col = "black", lty = 2, lwd = 1)
abline(h = 1, col = "black")
legend("topleft", c("Close price","Adjusted close price"),
       lty = c(1,2), lwd = c(2,1))

## Plot normalized prices with "close" and "adjusted close" prices of 2882
data_2882_np<-data_2882$Close/data_2882$Close[1]
data_2882_np1<-data_2882$Adj.Close/data_2882$Adj.Close[1] 

## Set range
rangex<-range(c(data_2882_np, data_2882_np1), na.rm = T)

plot(x = data_2882$Date, y = data_2882_np, ylim = rangex, 
     type = "l", xlab = "Date", ylab = "Value of Investment ($)", 
     col = "black", lty = 1, lwd = 2, cex.lab = 1.5, cex.axis = 1.2,
     main = "Value of $1 Investment in 2882, Dec-31-2013 ~ Dec-30-2022")
lines(x = data_2882$Date, y = data_2882_np1,
      col = "black", lty = 2, lwd = 1)
abline(h = 1, col = "black")
legend("topleft", c("Close price","Adjusted close price"),
       lty = c(1,2), lwd = c(2,1))

##--------------------------------------------------------------------------
## 10. Plot technical indicators for 3008
## load library "quantmod" and "xts"
## follow the procedures in the lecture slides
library(quantmod)
library(xts)

xts_3008<-xts(data_3008[,2:ncol(data_3008)], 
                order.by = data_3008$Date)
xts_3008_ohlc<-xts_3008[,c(1:4,6)]         ## Extract open, high, low, close and volume

## Check class
class(xts_3008_ohlc)

## Make the dates shown in English
## But you can avoid it if executing the codes makes errors in your program.
Sys.setlocale(category="LC_ALL",
              locale = "English_United States.1252")
windows(height = 8, width = 10) 
## Plot candlestick and volume
chartSeries(xts_3008_ohlc, name = "3008", theme = "white")

## SMA
addSMA(n = c(30, 150), col = c(1, 2))
addRSI(n = 30)
addMACD()