# ****************************************************************************************
# 
#   Rolling Regression Backtesting Script for Multiple Stocks with Portfolio Statistics
#
# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ******************************

library(rstudioapi)  
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")
options(scipen=999)  # removes scientific notation for numbers displayed

# ***************GET DATA AND SET TRADING DATE RANGE **********************************

library(forecast) 
library(zoo)
library(readr)
library(TTR)
sectors <- read_csv("sectors.csv")
load("SPUniverseHW3.rdata") 
universe<-stock
from<-as.Date("2020-01-01")
to<-as.Date("2023-12-3")
symbols <- subset(sectors,sectors$sector=='Energy')
symbolsEnergy<- as.vector(symbols$symbol)
numsymbols<-length(symbolsEnergy)
stock<-NULL
for (sym in 1:length(symbolsEnergy)){
  temp<-subset(universe,universe$symbol==symbolsEnergy[sym]&universe$date>=from&universe$date<=to)
  #if(nrow(temp)>500){
  if (is.null(stock)) {
    stock<-list(temp) 
  } else  {stock[[sym]]<-temp}
  }
}
is_non_null_list <- function(x) {
  is.list(x) && !is.null(x)
}
#Filter out the null lists
stock <- Filter(is_non_null_list, stock)
numsymbols<-length(stock)
# ************************************** GENERATE INDICATORS **********************************************************

genIndicators=function(stock){
  sym<-stock$symbol[1]
  print(paste('Generating Indicators for symbol: ',sym))
  tradedays<-nrow(stock)
  stock.ts<-ts(stock$close, start=c(1,1), end=c(1,tradedays), frequency=tradedays)  # convert to time series model
  stock$predictedclose <- NA
  stock$MA5 <- NA
  stock$MA8 <- NA
  stock$MA13 <- NA
  window<-27
  # Loop through the data and generate indicators
  for (i in window:tradedays-1) {
    # Calculate Moving Averages
    stock$MA5[i] <- mean(stock$close[(i-4):i])
    stock$MA8[i] <- mean(stock$close[(i-7):i])
    stock$MA13[i] <- mean(stock$close[(i-12):i])
    
    # Generate linear regression model
    lm_model <- lm(close ~ MA5 + MA8 + MA13, data = stock[(i-12):i,])
    
    # Predict next day's closing price
    predictors <- data.frame(MA5 = stock$MA5[i], MA8 = stock$MA8[i], MA13 = stock$MA13[i])
   # print(predict(lm_model, newdata = predictors))
    stock$predictedclose[i+1] <- predict(lm_model, newdata = predictors)
    
    #MACD Features
    ema12 <- EMA(stock$close, n = 12)
    ema26 <- EMA(stock$close, n = 26)
    
    # Calculate MACD line
    macd <- ema12 - ema26
    
    # Calculate 9-day exponential moving average of MACD line
    signal <- EMA(macd, n = 9)
    
    # Add MACD indicators to the timeseries object
    stock$macd <- macd
    stock$macd_signal <- signal
    
  }
  
  return(list(stock))
}

# **************************************** APPLY RULES ********************************************************************

applyRules=function(stock){
  sym<-stock$symbol[1]
  print(paste('Apply Rules for symbol: ',sym))
  stock$signal <- NA
  stock$long_short <- NA
  tradedays <- nrow(stock)
  window<-27
  for (i in c(window:tradedays)) {
    if (!any(is.na(c(stock$MA5[i], stock$MA8[i], stock$MA13[i], stock$macd[i], stock$macd_signal[i], stock$predicted_close[i], stock$close[i])))){
    if (((stock$MA5[i] > stock$MA8[i]) || (stock$MA8[i] > stock$MA13[i])) & (stock$macd[i] > stock$macd_signal[i])) {
      # Check if MACD line is above signal line
        stock$signal[i] <- "buy"
        stock$long_short[i] <- "long"
      
    } else if ((stock$MA5[i] < stock$MA8[i] || stock$MA8[i] < stock$MA13[i]) & (stock$macd[i] < stock$macd_signal[i])) {
      stock$signal[i] <- "sell"
      stock$long_short[i] <- "short"
    }
    }
  }
  
  #stock$signal <- zoo::na.locf(stock$signal, na.rm=FALSE)
  #stock$long_short <- zoo::na.locf(stock$long_short, na.rm=FALSE)
  
  # Generate the long/short columns based on the signals
  stock$trade <- ifelse(!is.na(stock$signal), 1, 0)
  stock$long <- ifelse(stock$long_short == "long", 1, 0)
  stock$short <- ifelse(stock$long_short == "short", 1, 0)
  stock$buy <- ifelse(stock$long == 1, stock$open, ifelse(stock$short == 1, stock$close, NA))
  stock$sell <- ifelse(stock$long == 1, stock$close, ifelse(stock$short == 1, stock$open, NA))
  stock$return <- ifelse(stock$trade == 1, 1 + (stock$sell - stock$buy) / stock$buy, 1)
  
  return(list(stock))
}


# ********************************* CALCULATE PERFORMANCE MEASURES *********************************************************

calcPerformance=function(stock){
  sym<-stock$symbol[1]
  tradedays<-nrow(stock)
  print(paste('Calculating Performance for symbol: ',sym))
  stock$cumreturn<-rep(1,tradedays)
  stock$maxreturn<-rep(1,tradedays)
  for (i in c(28:tradedays)){
    stock$cumreturn[i]<-prod(stock$return[c(1:i)],na.rm=TRUE)
    stock$maxreturn[i]<-max(stock$cumreturn[c(1:i)],na.rm=TRUE)
  }
  stock$down<-stock$cumreturn-stock$maxreturn
  stock$downpct<-stock$down/stock$maxreturn
  streak<-0
  maxstreak<-0
  for (i in c(1:tradedays)){
    streak<-ifelse(stock$down[i]<0,streak+1,0)
    maxstreak<-ifelse(streak>maxstreak,streak,maxstreak)
  }
  totaltrades<-sum(stock$trade,na.rm=TRUE)
  longtrades<-sum(stock$long,na.rm=TRUE)
  shorttrades<-sum(stock$short,na.rm=TRUE)
  cumreturn<-stock$cumreturn[tradedays]
  meanreturn<-mean(stock$return,na.rm=TRUE)-1
  sharpe<-meanreturn/sd(stock$return,na.rm=TRUE)*sqrt(252)
  maxdraw<-min(stock$downpct*100)
  plot(stock$cumreturn,type="l",col="black",lwd=2,xlab="Time Period",ylab="Equity Return",main=paste(sym,"Strategy Results"))
  lines(stock$maxreturn,co=2,lw=2)
  lines(stock$return,co=4,lw=2)
  legend(x=0,y=0.7,co=c(1,2,4),lw=c(2,2,2),legend=c("Max Return", "Cum Return","Daily Return"))
  performance<-list(totaltrades=totaltrades,longtrades=longtrades,shorttrades=shorttrades,cumreturn=cumreturn,
                    meanreturn=meanreturn,sharpe=sharpe,maxdraw=maxdraw,drawlength=maxstreak)
  return(performance)
}

# ****************************** CALCULATE PORTFOLIO STATISTICS ************************************************

portfolioStats=function(stock){
  tradedays<-nrow(data.frame(stock[[1]]))
  numsymbols<-length(stock)
  rvalues<-matrix(data=1,nrow=tradedays,ncol=numsymbols)
  totaltrades<-0
  shorttrades<-0
  longtrades<-0
  for (sym in 1:numsymbols) {
    instrument<-data.frame(stock[[sym]])
    totaltrades<-totaltrades+sum(instrument$trade,na.rm=TRUE)
    longtrades<-longtrades+sum(instrument$long,na.rm=TRUE)
    shorttrades<-shorttrades+sum(instrument$short,na.rm=TRUE)
    for (period in 1:tradedays) {
      rvalues[period,sym]<-ifelse(instrument$trade[period]==1,instrument$return[period],1)
    }
  }
  return<-apply(rvalues,1,prod)
  cumreturn<-rep(1,tradedays)
  maxreturn<-rep(1,tradedays)
  for (i in c(1:tradedays)){
    cumreturn[i]<-prod(return[c(1:i)],na.rm=TRUE)
    maxreturn[i]<-max(cumreturn[c(1:i)],na.rm=TRUE)
    print(cumreturn[i])
  }
  down<-cumreturn-maxreturn
  downpct<-down/maxreturn
  streak<-0
  maxstreak<-0
  for (i in c(1:tradedays)){
    streak<-ifelse(down[i]<0,streak+1,0)
    maxstreak<-ifelse(streak>maxstreak,streak,maxstreak)
  }
  plot(cumreturn,type="l",col="black",lwd=2,xlab="Time Period",
       ylab="Portfolio Return",main="Portfolio Results")
  lines(maxreturn,co=2,lw=2)
  lines(return,co=4,lw=2)
  legend(x=500,y=0.75,co=c(1,2,4),lw=c(2,2,2),
         legend=c("Cum Return","Max Return","Daily Return"))
  cumreturn<-cumreturn[tradedays]
  meanreturn<-mean(return,na.rm=TRUE)-1
  sharpe<-meanreturn/sd(return,na.rm=TRUE)*sqrt(252)
  maxdraw<-min(downpct*100)
  performance<-list(totaltrades=totaltrades,longtrades=longtrades,
                    shorttrades=shorttrades,cumreturn=cumreturn,
                    meanreturn=meanreturn,sharpe=sharpe,maxdraw=maxdraw,
                    drawlength=maxstreak)
  return(performance)
}

# ********************************  RUN STRATEGY **********************************
    
for (sym in 1:numsymbols) {
  stock[[sym]]<-genIndicators(data.frame(stock[[sym]]))
  stock[[sym]]<-applyRules(data.frame(stock[[sym]]))
  if (sym==1){
    results<-calcPerformance(data.frame(stock[[sym]]))
  } else {
    results[[sym]]<-calcPerformance(data.frame(stock[[sym]]))
  }
}

for (i in 1:numsymbols) {
  print(results[[i]])
}

performance<-portfolioStats(stock) 
performance
  


