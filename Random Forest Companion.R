# ****************************************************************************************
# Random Forest lab using the universe data set.  Pick a day with a window of a given size
# and train a regression random forest to predict returns
# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ******************************
library(rstudioapi)
library(dplyr)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
options(scipen=999)
cat("\014")
# ***************GET DATA AND SET TRADING DATE RANGE **************************************
library(quantmod)
library(randomForest)
load("universe.rdata")
days<-unique(universe$date)
days<-days[order(days)]
windowsize<-10
dataend<-2000
longestindicator<-40
datastart<-dataend-longestindicator-windowsize+1
trainstart<-dataend-windowsize
trainend<-dataend
universe<-subset(universe,universe$date>=days[datastart])
universe<-subset(universe,universe$date<=days[dataend])
symbols<-unique(universe$symbol)
numsymbols<-length(symbols)

# ************************************** GENERATE INDICATORS **********************************************************
genIndicators=function(sym){
  print(paste('Generating Indicators for symbol: ',sym))
  stock<-subset(universe,universe$symbol==sym)                # work with one symbol at a time
  stock.xts<-xts(stock[,c(3:7)],stock$date)                   # convert to xts object 
  stock.xts$nreturn<-(lead(as.vector(stock.xts$close),1)-lead(as.vector(stock.xts$open),1))/lead(as.vector(stock.xts$open),1)+1
  stock.xts$momentum1<-diff(stock.xts$close)  
  stock.xts$accel<-diff(stock.xts$momentum1)
  stock.xts$momentum10<-tryCatch({
  stock.xts$momentum10<-momentum(stock.xts$close,n=10)        # sometimes calls to TTR functions will crash the system   
  }, warning=function(w) {stock.xts$momentum10<-NA }, error=function(e) {stock.xts$momentum10<-NA})
  stock.xts$momentum20<-tryCatch({
  stock.xts$momentum20<-momentum(stock.xts$close,n=20)        # sometimes calls to TTR functions will crash the system   
  }, warning=function(w) {stock.xts$momentum20<-NA }, error=function(e) {stock.xts$momentum20<-NA})
  macd<-tryCatch({
    macd<-MACD(stock.xts$close,maType="EMA")                  # sometimes calls to TTR functions will crash the system   
    }, warning=function(w) {macd<-NULL }, error=function(e) {macd<-NULL})
    if (is.null(macd)) {
    stock.xts$macdDiff<-NA
  } else stock.xts$macdDiff<-macd[,1]-macd[,2]                    # our indicator
  stock.xts$sma5<-tryCatch({
    stock.xts$sma5<-SMA(stock.xts$close,n=5)        # sometimes calls to TTR functions will crash the system   
  }, warning=function(w) {stock.xts$sma5<-NA }, error=function(e) {stock.xts$sma5<-NA})
  stock.xts$sma10<-tryCatch({
    stock.xts$sma10<-SMA(stock.xts$close,n=10)        # sometimes calls to TTR functions will crash the system   
  }, warning=function(w) {stock.xts$sma10<-NA }, error=function(e) {stock.xts$sma10<-NA})
  stock.xts$sma20<-tryCatch({
    stock.xts$sma20<-SMA(stock.xts$close,n=20)        # sometimes calls to TTR functions will crash the system   
  }, warning=function(w) {stock.xts$sma20<-NA }, error=function(e) {stock.xts$sma20<-NA})
  stock.xts$sma40<-tryCatch({
    stock.xts$sma40<-SMA(stock.xts$close,n=40)        # sometimes calls to TTR functions will crash the system   
  }, warning=function(w) {stock.xts$sma40<-NA }, error=function(e) {stock.xts$sma40<-NA})
  stock.xts$cross510<-stock.xts$sma5/stock.xts$sma10
  stock.xts$cross520<-stock.xts$sma5/stock.xts$sma20
  stock.xts$cross540<-stock.xts$sma5/stock.xts$sma40
  stock.xts$cross1020<-stock.xts$sma10/stock.xts$sma20
  stock.xts$cross1040<-stock.xts$sma10/stock.xts$sma40  
  stock.xts$rsi5<-tryCatch({
    stock.xts$rsi5<-RSI(stock.xts$close,n=5)        # sometimes calls to TTR functions will crash the system   
  }, warning=function(w) {stock.xts$rsi5<-NA }, error=function(e) {stock.xts$rsi5<-NA})
  stock.xts$rsi10<-tryCatch({
    stock.xts$rsi10<-RSI(stock.xts$close,n=10)        # sometimes calls to TTR functions will crash the system   
  }, warning=function(w) {stock.xts$rsi10<-NA }, error=function(e) {stock.xts$rsi10<-NA})
  stock.xts$rsi20<-tryCatch({
    stock.xts$rsi20<-RSI(stock.xts$close,n=20)        # sometimes calls to TTR functions will crash the system   
  }, warning=function(w) {stock.xts$rsi20<-NA }, error=function(e) {stock.xts$rsi20<-NA})
  stock.xts<-na.omit(stock.xts)
  maxs <- apply(stock.xts, 2, max)
  mins <- apply(stock.xts, 2, min)
  scaled.xts<-tryCatch({
    scaled.xts<-scale(stock.xts,center = mins, scale = maxs - mins)           
  }, warning=function(w) {scaled.xts<-NULL }, error=function(e) {scaled.xts<-NULL})
  
  if (!is.null(scaled.xts)){
    stock<-data.frame(scaled.xts)                                # convert back to dataframe object
    stock$nreturn<-(lead(as.vector(stock.xts$close),1)-lead(as.vector(stock.xts$open),1))/lead(as.vector(stock.xts$open),1)+1
    date<-as.Date(rownames(stock))                              
    stock<-cbind(date,stock)                                    
    stock$dow<-as.factor(weekdays(stock$date,abbreviate=TRUE))
    stock$symbol<-sym
    stock<-stock[,c(26,1:25)]
    for (i in (3:ncol(stock))){
      inflist<-which(is.infinite(stock[,i]))
      if (!is.null(inflist)) {
        for (j in inflist)
          stock[j,i]<-NA
      }
    }
    stock<-na.omit(stock)
    rownames(stock)<-seq(1,nrow(stock),1)
  } else stock<-NULL
  return(stock)
}

# ***********************************  GENERATE INDICATORS **********************************************
indicators<-NULL                               # we will put all OHLC data and our generated
for (sym in symbols) {                         # indicators into a dataframe named "indicators"
  temp<-genIndicators(sym)                     # by looping through all the symbols in our
  if (!is.null(temp)) {                        # restricted universe. Need to ensure we have indicators  
    if (is.null(indicators)) {                   
      indicators<-temp} else
        indicators<-rbind(indicators,temp)
  }
}

# *********************************** GENERATE PREDICTIONS **********************************************
rf.data<-indicators[,c(3:26)]
set.seed(25)
train<-sample(1:nrow(rf.data), nrow(rf.data)*0.75)
test=(1:nrow(rf.data))[-train]

rf.model=randomForest(nreturn~.,data=rf.data[train,], mtry=24,importance=FALSE)
rpredict<-predict(rf.model,rf.data[test,])
sqrt(mean((rpredict-rf.data[test,"nreturn"])^2))
summary(rpredict)
summary(rf.data[test,"nreturn"])

