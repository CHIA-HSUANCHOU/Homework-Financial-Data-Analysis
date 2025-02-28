## Exercise 4 R program script
## 1.
rm(list = ls(all = TRUE))
##import data  
data_2317<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW4/2317.TW.csv", sep= ",", na.strings = "null", header = T)
data_2330<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW4/2330.TW.csv", sep= ",", na.strings = "null", header = T)
data_2412<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW4/2412.TW.csv", sep= ",", na.strings = "null", header = T)
data_2882<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW4/2882.TW.csv", sep= ",", na.strings = "null", header = T)
data_3008<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW4/3008.TW.csv", sep= ",", na.strings = "null", header = T)

## Transform Date with as.Date
data_2317[,1]<-as.Date(data_2317[,1])
data_2330[,1]<-as.Date(data_2330[,1])
data_2412[,1]<-as.Date(data_2412[,1])
data_2882[,1]<-as.Date(data_2882[,1])
data_3008[,1]<-as.Date(data_3008[,1])

## Extract data from 2014-12-31 ~ 2023-12-29
data_2317<-data_2317[data_2317$Date>="2014-12-31"&data_2317$Date<="2023-12-29",]
data_2330<-data_2330[data_2330$Date>="2014-12-31"&data_2330$Date<="2023-12-29",]
data_2412<-data_2412[data_2412$Date>="2014-12-31"&data_2412$Date<="2023-12-29",]
data_2882<-data_2882[data_2882$Date>="2014-12-31"&data_2882$Date<="2023-12-29",]
data_3008<-data_3008[data_3008$Date>="2014-12-31"&data_3008$Date<="2023-12-29",]

## How many samples in each stock?
dim(data_2317); dim(data_2330); dim(data_2412); dim(data_2882);dim(data_3008)
#[1] 2197    7

## How many NA's in Adj.Close of each stock
sum(is.na(data_2317[,6]))
sum(is.na(data_2330[,6]))
sum(is.na(data_2412[,6]))
sum(is.na(data_2882[,6]))
sum(is.na(data_3008[,6]))

## Clean data
## For dealing with the missing values, try to write a function to 
## replace NA's with their previous values
NA_rep<-function(x, colx){                       ## x: a data frame, colx: which column of the data frame
    
    ind_miss<-which(is.na(x[, colx]))            ## where the missing data is? 
    
    for(i in ind_miss){                           ## use a "for" loop to replace the missing value with 
                                                 ## its previous one
      x[i, colx]<-x[(i-1), colx]
      
    }
    
    return(x)
    
}

## Replace NA of Adj.close with its previous value by using the function NA_rep  
data_2317<-NA_rep(data_2317, 6)  
data_2330<-NA_rep(data_2330, 6)   
data_2412<-NA_rep(data_2412, 6) 
data_2882<-NA_rep(data_2882, 6)  
data_3008<-NA_rep(data_3008, 6)  

## Check whether there is any NA
sum(is.na(data_2317[,6])) #0
sum(is.na(data_2330[,6])) #0
sum(is.na(data_2412[,6])) #0
sum(is.na(data_2882[,6])) #0
sum(is.na(data_3008[,6])) #0
## ----------------------------------------------------------------------------------------------
## Calculate each stock's return
## Need to use the function retx in the R script "function_FDA.R" used in class  
source("C:/Users/chouchiahsuan/Desktop/財務資訊分析/Lec4/function_FDA.R")

## Calculate returns
data_2317$ret<-c(NA, retx(data_2317$Adj.Close))  
data_2330$ret<-c(NA, retx(data_2330$Adj.Close)) 
data_2412$ret<-c(NA, retx(data_2412$Adj.Close)) 
data_2882$ret<-c(NA, retx(data_2882$Adj.Close))  
data_3008$ret<-c(NA, retx(data_3008$Adj.Close)) 

## Collect the return data
datax<-data.frame(matrix(0, nrow(data_2317), 6))
datax[,1]<-data_2317$Date
datax[,2:ncol(datax)]<-cbind(data_2317$ret, data_2330$ret, 
                             data_2412$ret, data_2882$ret,data_3008$ret)  

colnames(datax)<-c("Date","x2317","x2330","x2412",
                   "x2882","x3008")    

head(datax)
## -----------------------------------------------------------------------------------
## a.
## Calculate portfolio returns
## Fixed-weighted, 1/N
datax$retN<-apply(datax[, 2:ncol(datax)], 1, mean, na.rm = T)   ##note to set na.rm = T
datax$retN<-datax$retN*100                                                  ##show the returns in percentage    

## Buy and hold
## Creat a new data frame "rx"
rx<-rbind(0, datax[-1, 2:(ncol(datax)-1)])                  ## note that remove the first row since it contains NA's
rx<-rx+1                                                    ## calculate gross return of each stock
bh_cumr<-apply(rx, 2, cumprod)                        ## calculate cumulative return of each stock
bh_cumr<-apply(bh_cumr, 1, mean)                         ## calculate average of the cumulative returns
datax$retbh<-c(NA, bh_cumr[-1]/bh_cumr[-length(bh_cumr)]-1)    ## calculate return of the buy-and-hold portfolio
datax$retbh<-datax$retbh*100                             ## show the returns in percentage
head(datax)

## Remove the first row of NA
datax<-datax[-1, ]

## Calculate summary statistics
apply(datax[, 7:8], 2, summary)                 ## summary statistic
apply(datax[, 7:8], 2, sd)           ## sample standard deviation
apply(datax[, 7:8], 2, my_skewness)           ## sample skewness with function skewnessx used in class
apply(datax[, 7:8], 2, my_kurtosis)            ## sample excess kurtosis with kurtosisx used in class
apply(datax[, 7:8], 2, my_acf1)            ## acf(1) with function acf1 used in class
## ----------------------------------------------------------------------------
## b.
## Calculate VaR and ES with the whole sample with function VaR_samplex and 
## ES_samplex used in R codes of lecture 4 
  
## Alpha = 0.05  
apply(datax[, 7:8], 2, VaR_samplex, amountx = 1, alphax = 0.05)   
apply(datax[, 7:8], 2, ES_samplex, amountx = 1, alphax = 0.05)

## Alpha = 0.01  
apply(datax[, 7:8], 2, VaR_samplex, amountx = 1, alphax = 0.01)   
apply(datax[, 7:8], 2, ES_samplex, amountx = 1, alphax = 0.01)

## -------------------------------------------------------------------------------
## c.  
## Calculate VaR and ES with expanding and rolling window
## fixed-weighted portfolio
kx<-250                                      ## window length  
alpha<-0.05                                   ## VaR level
result1<-NULL
result2<-NULL

## Use a "for" loop
for(i in 1:(nrow(datax)-kx)){        
  
  ## Expanding window
  varx<-VaR_samplex(datax[1:(i+kx-1), 7],  1, alpha)         
  esx<-ES_samplex(datax[1:(i+kx-1), 7], 1, alpha) 
  result1<-rbind(result1, c(varx, esx))
  
  ## Rolling window
  varx1<-VaR_samplex(datax[i:(i+kx-1), 7], 1, alpha)          
  esx1<-ES_samplex(datax[i:(i+kx-1), 7], 1, alpha)           
  result2<-rbind(result2, c(varx1, esx1))
  
}

## Collect the revevant results: portfolio returns, VaR and ES
datax1<-data.frame(matrix(0, nrow(datax)-kx, 6))
datax1[,1]<-datax$Date[(kx+1):nrow(datax)]
datax1[,2]<-datax[(kx+1):nrow(datax), 7]                            ## portfolio returns in percentage
datax1[,3:ncol(datax1)]<-cbind(result1, result2)                    ## VaR and ES

colnames(datax1)<-c("Date","r","VaR_Exp","ES_Exp","VaR_Rw","ES_Rw")

## Obtain range of the data for plotting
rangex<-range(datax1[, 2:ncol(datax1)])

## Time series plot
windows(height = 8, width = 10)
plot(x = datax1$Date, y = datax1$r, type="l", main = "Alpha = 0.05",
     ylim = rangex, col = "gray",
     xlab = "Date", ylab = "Return (%)", cex.axis = 1.5, cex.lab = 1.5)
lines(x = datax1$Date, y = datax1$VaR_Exp, type="l", col = 1)         ## add the line of VaR_Exp 
lines(x = datax1$Date, y = datax1$ES_Exp, type="l", col = 2)       ## add the line of ES_Exp 
lines(x = datax1$Date, y = datax1$VaR_Rw, type="l", col = 3)            ## add the line of VaR_RW 
lines(x = datax1$Date, y = datax1$ES_Rw, type="l", col = 4)          ## add the line of ES_RW 
legend("topleft", 
       legend = c("Return","VaR_EXP","ES_EXP", "VaR_RW", "ES_RW"), 
       lty =c(1,1,1,1,1),
       col = c("gray","black","red","green","blue"))
--------------------------------------------------------------------------------------
## Buy-and-hold portfolio
kx<-250                                      ## window length  
alpha<-0.05                                 ## VaR level

result1<-NULL
result2<-NULL

## Use a "for" loop
for(i in 1:(nrow(datax)-kx)){        
  
  ## Expanding window
  varx<-VaR_samplex(datax[1:(i+kx-1), 8], 1, alpha)         
  esx<-ES_samplex(datax[1:(i+kx-1), 8], 1, alpha)  
  result1<-rbind(result1, c(varx, esx))
  
  ## Rolling window
  varx1<-VaR_samplex(datax[i:(i+kx-1), 8], 1, alpha)          
  esx1<-ES_samplex(datax[i:(i+kx-1), 8], 1, alpha)          
  result2<-rbind(result2, c(varx1, esx1))
  
}

## Collect the revevant results: portfolio returns, VaR and ES
datax2<-data.frame(matrix(0, nrow(datax)-kx, 6))
datax2[,1]<-datax$Date[(kx+1):nrow(datax)]
datax2[,2]<-datax[(kx+1):nrow(datax), 8]                             ## portfolio returns in percentage
datax2[,3:ncol(datax2)]<-cbind(result1, result2)                                ## VaR and ES

colnames(datax2)<-c("Date","r","VaR_Exp","ES_Exp","VaR_Rw","ES_Rw")

## Obtain range of the data for plotting
rangex<-range(datax2[, 2:ncol(datax2)])

## Time series plot
windows(height = 8, width = 10)
plot(x = datax2$Date, y = datax2$r, type="l", main = "Alpha = 0.05",
     ylim = rangex, col = "gray",
     xlab = "Date", ylab = "Return (%)", cex.axis = 1.5, cex.lab = 1.5)

lines(x = datax2$Date, y = datax2$VaR_Exp, type="l", col = 1)             ## add the line of VaR_Exp
lines(x = datax2$Date, y = datax2$ES_Exp, type="l", col = 2)                       ## add the line of ES_Exp
lines(x = datax2$Date, y = datax2$VaR_Rw, type="l", col = 3)                      ## add the line of VaR_RW
lines(x = datax2$Date, y = datax2$ES_Rw, type="l", col = 4)                      ## add the line of ES_RW

## Put the legend on the top left corner
legend("topleft",                                    
       legend = c("Return","VaR_EXP","ES_EXP", "VaR_RW", "ES_RW"), 
       lty = c(1,1,1,1,1),
       col = c("gray","black","red","green","blue"))
## ------------------------------------------------------------------------------
## d. 
## Realized exceed
## Fixed-weighted portfolio  
sum(datax1$r<datax1$VaR_Exp)
sum(datax1$r<datax1$VaR_Rw)

## Buy-and-hold portfolio
sum(datax2$r<datax2$VaR_Exp)
sum(datax2$r<datax2$VaR_Rw)

## -----------------------------------------------------------------------
## e.
## Calculate MDD and Calmar ratio
library(PerformanceAnalytics)
library(xts)

xts_datax<-xts(datax[, 7:8], order.by = datax$Date)
xts_datax1<-xts_datax[index(xts_datax)>="2019-01-01"&index(xts_datax)<="2022-12-31",]

## Note that you cannot use "percentage" return here!!
xts_datax1<-xts_datax1/100

## Calculate MDD
## Fixed-weighted portfolio
N_MDD<-maxDrawdown(xts_datax1$retN)              
N_MDD

## Buy-and-hold portfolio
bh_MDD<-maxDrawdown(xts_datax1$retbh)                         
bh_MDD

## Calculate Calmar ratio
## Fixed-weighted portfolio
N_CR<-CalmarRatio(xts_datax1$retN)                    
N_CR

## Buy-and-hold portfolio
bh_CR<-CalmarRatio(xts_datax1$retbh)             
bh_CR

## -------------------------------------------------------------------------------------
## 2.
## Compare RV and VIX of SP500 index
rm(list = ls(all = TRUE))
library(xts)

source("C:/Users/chouchiahsuan/Desktop/財務資訊分析/Lec4/function_FDA.R")

GSPCd<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW4/GSPCd_010290_042920.csv", sep= ",", na.strings = "null", header = T)
VIXm<-read.table("C:/Users/chouchiahsuan/Desktop/財務資訊分析/HW4/VIXm_0190_0420.csv", sep= ",", na.strings = "null", header = T)

GSPCd$Date<-as.Date(GSPCd$Date)
VIXm$Date<-as.Date(VIXm$Date)
## --------------------------------------------------------------------------------
## a.
## Number of observations
nrow(GSPCd)
nrow(VIXm)

## Number of missing values
sum(is.na(GSPCd)) #0
sum(is.na(VIXm)) #0

## --------------------------------------------------------------------------------
## b.
## Calculate summary statistics
summary(GSPCd)
summary(VIXm)

## -------------------------------------------------------------------------------
## c.  
## Calculate monthly RV of SP500
## Collect date and adjusted close price
xts_GSPCd<-xts(GSPCd$Adj.Close, order.by = GSPCd$Date)
colnames(xts_GSPCd)<-"Adjusted"

d.adj<-as.numeric(xts_GSPCd$Adjusted)

## Calculate daily returns and daily squared returns
xts_GSPCd$ret<-c(NA, retx(d.adj))
xts_GSPCd$ret2<-xts_GSPCd$ret^2                        

## Calculate monthly RV
rvm<-apply.monthly(xts_GSPCd$ret2, sum, na.rm = T) 

## Calculate and scale the realized volatility
## in a appropriate way
rvolm<-sqrt(rvm)*sqrt(12)*100

## --------------------------------------------------------------------------------------
## d.
## Calculate volatility risk premium
vrpm<-VIXm$Close-as.numeric(rvolm)

## Use summary and calculate 
## How many positive and nonnegative volatility premium?
summary(vrpm)
sum(vrpm<0)
sum(vrpm>=0)

## --------------------------------------------------------------------------------------------
## e.
## Plot time series plots of monthly realized volatility and VIX
windows(height = 8, width = 10)
par(mfrow = c(2,1))


plot(x = as.Date(index(rvolm)), y = as.numeric(rvolm), 
     main = "Monthly realized volatility and VIX of S&P500 (annualized percentage)",
     xlab = "Date", ylab = "", type = "l",
     lwd = 2,
     cex.main = 1.2, cex.lab = 1.5, cex.axis = 1.5)

lines(x = VIXm$Date, y = VIXm$Close, 
      col = 2, lwd = 1)

legend("topleft", legend = c("RVOL", "VIX"), col = c(1,2),
       lty = c(1,1), lwd = c(2,1))


## Plot time series plots of volatility risk premium
plot(x = VIXm$Date, y = vrpm, 
     main = "Monthly volatility risk premium of S&P500 (annualized percentage)",
     xlab = "Date", ylab = "", type = "h",
     lwd = 2, col = 2,
     cex.main = 1.2, cex.lab = 1.5, cex.axis = 1.5)

