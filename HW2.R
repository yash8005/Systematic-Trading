library(rstudioapi)  # This is a external library of functions
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
cat("\014")
install.packages('Quandl')
library(Quandl)                      
Quandl.api_key('EfNYF1EymebW8saMFp5B')  

# Get 5 years of S&P Data through the Quandl API

symbols<-as.vector(read.csv("SP Tickers.csv")[,1])
symbols <- unique(symbols)
fromdate=as.Date("2018-02-01")
if(!file.exists('SPUniverse.rdata')){
  firsttime<-TRUE
  for (currsymbol in symbols) {
    print(c(currsymbol))
    temp<-tryCatch({
      temp<-Quandl.datatable("SHARADAR/SEP", date.gte=fromdate,ticker=currsymbol)  
    }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
    if (!is.null(temp)) {
      if (firsttime) {
        stock<-temp
      } else {
        stock<-rbind(stock,temp)}
      firsttime<-FALSE
    }
  }

  names(stock)[1]<-"symbol"
  
  # Get rid of of data prior to date stock is added to the S&P 500
  
  add<-read.csv("SP additions.csv")
  add$date.added<-as.Date(add$date.added,format="%m/%d/%Y")
  temp<-merge(stock,add,all.x=TRUE)
  temp$date.added<-as.Date(ifelse(is.na(temp$date.added),as.Date("2000-12-31"),temp$date.added))
  temp<-subset(temp,temp$date>=temp$date.added)
  
  # Get rid of data after stock is removed from the S&P 500
  
  remove<-read.csv("SP removals.csv")
  remove$date.removed<-as.Date(remove$date.removed,format="%m/%d/%Y")
  temp<-merge(temp,remove,all.x=TRUE)
  temp$date.removed<-as.Date(ifelse(is.na(temp$date.removed),as.Date("2100-12-31"),temp$date.removed))
  temp<-subset(temp,temp$date<temp$date.removed)
  temp<-temp[order(temp$symbol,temp$date),]
  stock<-temp[,c(1:9)]
  
  rownames(stock)<-seq(1,nrow(stock),1)
  save(stock,file="SPUniverse.rdata")
} else {
    load('SPUniverse.rdata')
}

#ANS 1
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

library('tidyverse')
library('dplyr')
symbols<-as.vector(read.csv("SP Tickers.csv")[,1])
symbols <- unique(symbols)
fromdate=as.Date("2018-02-01")
load('SPUniverse.rdata')

if(file.exists("insiderTradingEQ.rdata")){
  load('insiderTradingEQ.rdata')
} else {
fromdate=as.Date("2018-01-02")
insiderTradingEQ <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(insiderTradingEQ) <- c('ticker', 'transactiondate','sharesboughtEQ', 'sharessoldEQ', 'numownersEQ') 
for (symbol in symbols) {
  print(symbol)
  insiderTrading<-Quandl.datatable("SHARADAR/SF2",ticker=symbol,filingdate.gte=fromdate, securityadcode=c('N','NA','ND'),paginate=TRUE)
  insiderTrading<-insiderTrading[!is.na(insiderTrading$transactiondate),]
  
  groupedInsiderTrading <- insiderTrading %>% group_by(transactiondate) %>%
    summarize(sharesboughtEQ=sum(ifelse(transactionshares > 0, transactionshares, 0)), 
              sharessoldEQ=sum(ifelse(transactionshares < 0, -transactionshares, 0)), 
              numownersEQ=n_distinct(ownername))
  groupedInsiderTrading$ticker <- symbol
  
  groupedInsiderTrading <- groupedInsiderTrading[c('ticker', 'transactiondate','sharesboughtEQ', 'sharessoldEQ', 'numownersEQ')]
  insiderTradingEQ<-rbind(insiderTradingEQ,groupedInsiderTrading)
}
save(insiderTradingEQ,file="insiderTradingEQ.rdata")
}
if(file.exists("insiderTradingRSU.rdata")){
load('insiderTradingRSU.rdata')
} else{
fromdate=as.Date("2018-01-02")
insiderTradingRSU <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(insiderTradingRSU) <- c('ticker', 'transactiondate','sharesboughtRSU', 'sharessoldRSU', 'numownersRSU')
for (symbol in symbols) {
  print(symbol)
  insiderTrading<-Quandl.datatable("SHARADAR/SF2",ticker=symbol,filingdate.gte=fromdate, securityadcode=c('D','DA','DD'))
  insiderTrading<-insiderTrading[!is.na(insiderTrading$transactiondate),]
  
  groupedInsiderTrading <- insiderTrading %>% group_by(transactiondate) %>%
    summarize(sharesboughtRSU=sum(ifelse(transactionshares > 0, transactionshares, 0)), 
              sharessoldRSU=sum(ifelse(transactionshares < 0, -transactionshares, 0)), 
              numownersRSU=n_distinct(ownername))
  groupedInsiderTrading$ticker <- symbol
  
  groupedInsiderTrading <- groupedInsiderTrading[c('ticker', 'transactiondate','sharesboughtRSU', 'sharessoldRSU', 'numownersRSU')]
  insiderTradingRSU<-rbind(insiderTradingRSU,groupedInsiderTrading)
}
  save(insiderTradingRSU,file="insiderTradingRSU.rdata")
}

stock <- merge(x=stock,y=insiderTradingEQ, by.x=c("symbol","date"), 
               by.y=c("ticker","transactiondate"),all.x = TRUE, all.y=FALSE)
stock <- merge(x=stock,y=insiderTradingRSU, by.x=c("symbol","date"), 
               by.y=c("ticker","transactiondate"),all.x=TRUE)

save(stock,file="SPUniverseWithInsider.rdata")


#ANS 2
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
library(tidyverse,dplyr,Quandl)
symbols<-as.vector(read.csv("SP Tickers.csv")[,1])
symbols <- unique(symbols)
fromdate=as.Date("2018-02-01")
load('SPUniverseWithInsider.rdata')

if(file.exists("stockDailyFundamental.rdata")){
  load("stockDailyFundamental.rdata")
} else {
    firsttime<-TRUE
    for (currsymbol in symbols) {
        print(c(currsymbol))
        temp<-tryCatch({
          temp<-Quandl.datatable("SHARADAR/DAILY", date.gte=fromdate,ticker=currsymbol)  
        }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
        if (!is.null(temp)) {
          if (firsttime) {
            stockDaily<-temp
          } else {
            stockDaily<-rbind(stockDaily,temp)}
          firsttime<-FALSE
        }
      }
    
    save(stockDaily,file="stockDailyFundamental.rdata")
}

stock <- merge(x=stock,y=stockDaily, by.x=c("symbol","date"), 
               by.y=c("ticker","date"),all.x=TRUE,all.y=FALSE)

save(stock,file="SPuniverseWithInsiderFundamental.rdata")

load('SPuniverseWithInsiderFundamental.rdata')

if(!file.exists("SPUniverseWithInsiderFundamentalDividend.rdata")){
  firsttime<-TRUE
  for (currsymbol in symbols) {
    print(c(currsymbol))
    temp<-tryCatch({
      temp<-Quandl.datatable("SHARADAR/ACTIONS", date.gte=fromdate,ticker=currsymbol,action='dividend')  
    }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
    if (!is.null(temp)) {
      if (firsttime) {
        stockCorporateActions<-temp
      } else {
        stockCorporateActions<-rbind(stockCorporateActions,temp)}
      firsttime<-FALSE
    }
  }
  
  stockCorporateActions <- stockCorporateActions[-c(2,4,6,7)]
  colnames(stockCorporateActions)[3] <- "dividendValue"
  stock <- merge(x=stock,y=stockCorporateActions, by.x=c("symbol","date"), 
                 by.y=c("ticker","date"),all.x=TRUE,all.y=FALSE)
  
  save(stock,file="SPUniverseWithInsiderFundamentalDividend.rdata")
} else {
  load("SPUniverseWithInsiderFundamentalDividend.rdata")
}




symbols<-as.vector(read.csv("SP Tickers.csv")[,1])
symbols <- unique(symbols)
fromdate=as.Date("2018-02-01")
load('SPUniverse.rdata')

if(file.exists("fundamental.rdata")){
  load('fundamental.rdata')
} else {
  fromdate=as.Date("2018-01-02")
  for (symbol in symbols) {
    print(symbol)
    insiderTrading<-Quandl.datatable("SHARADAR/SF1",ticker=symbol,calendardate.gte=fromdate,paginate=TRUE)
    insiderTrading<-insiderTrading[!is.na(insiderTrading$calendardate),]
  }
  save(insiderTrading,file="fundamental.rdata")
}