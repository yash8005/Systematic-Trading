# ****************************************************************************************
# 
#   Rolling Regression Backtesting Script for Multiple Stocks with Portfolio Statistics & Portfolio Weighting
#
# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ******************************

library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
options(scipen=999)

# ***************GET DATA AND SET TRADING DATE RANGE **********************************

library(forecast) 
library(zoo)
library(readr)
library(TTR)
library(xts)
load('sectors.rdata')
#SP Universe has all the data from 2018 to 2023 without Addition or Removal
load("SPUniverse.rdata") 
universe<-stock
from<-as.Date("2021-01-01")
to<-as.Date("2022-12-31")
symbolsUE <- subset(sectors,sectors$sector=='Energy')
symbols<- as.vector(symbolsUE$symbol)
stock<-NULL
numsymbols<-length(symbols)
window<-15
initialequity<-100000
maxdaytrades<-floor(numsymbols/2)
maxtrade<-((initialequity*0.9)/maxdaytrades)*(4)
returnthreshold<-0.0025
rangethreshold<-0.02
# ************************************** GENERATE INDICATORS **********************************************************
genIndicators=function(sym,window){
  print(paste('Generating Indicators for symbol: ',sym))
  stock<-subset(universe,universe$symbol==sym&universe$date>=(from-(2*window))&universe$date<=to)
  stock <- stock[order(as.Date(stock$date, format="%m/%d/%Y")),]
  if(nrow(stock)>10){
    # Find the maximum date in the dataframe that is less than or equal to the specific date
    max_date <- max(stock$date[stock$date <= from])
    # Find the index of the maximum date
    max_date_index <- which(stock$date == max_date)
    if (max_date_index >= window) {
      # If max_date_index is greater than or equal to 30, extract the 30 rows before it

      stock <- stock[(max_date_index-window-1):nrow(stock), ]
    } else {
      # If max_date_index is less than 30, extract data starting from the first row of the dataframe

      stock <- stock[1:nrow(stock), ]
    }
  }
  else{
    return (data.frame());
  }
  
  stock.xts<-xts(stock[,c(3:7)],stock$date)
  stock.xts$predictreturn<-NA
  stock.xts$predictrpct<-NA
  stock.xts$tradetype<-NA
  
  for (i in window:(nrow(stock.xts)-1)) {
     lm_model<-tryCatch({

       MA5 <- mean(stock.xts$close[(i-4):i])
       MA8 <- mean(stock.xts$close[(i-7):i])
       MA13 <- mean(stock.xts$close[(i-12):i])

       # Generate linear regression model
       lm_model <- lm(close ~ open + high + low, data = stock.xts[(i-window+1):i,])

       # Predict next day's closing price
       predictors <- data.frame(open = stock.xts$open[i], high = stock.xts$high[i], low = stock.xts$low[i])

       predictionsummary <- predict(lm_model, newdata = predictors,interval="confidence")

       # Use TryCatch to handle the error
     }, warning=function(w) {lm_model<-NULL }, error=function(e) {lm_model<-NULL})
     if (is.null(lm_model))  {

     }
     if (!is.null(lm_model)) {
       if ((MA5 > MA8) & (MA8 > MA13)) {
            stock.xts$tradetype[i+1]=1
            stock.xts$predictreturn[i+1]<-(predictionsummary[,1]-stock.xts$close[i])/
              stock.xts$close[i]
       } else if ((MA5 < MA8) & (MA8 < MA13)) {
          stock.xts$tradetype[i+1]=-1
          stock.xts$predictreturn[i+1]<-(stock.xts$close[i]-predictionsummary[,1])/
            predictionsummary[,1]
         
       }
       stock.xts$predictrpct[i+1]<-((predictionsummary[,3] - predictionsummary[,2]))/
         predictionsummary[,1]
     }
  }

 stock<-data.frame(stock.xts)
 date<-as.Date(rownames(stock))                              
 stock<-cbind(date,stock)                                    
 stock$symbol<-sym
 stock<-stock[,c(10,1:9)]
 rownames(stock)<-seq(1,nrow(stock),1)  

 stock<-subset(stock,stock$date>=(from)&stock$date<=to)
 return(stock)
}


 # **************************************** APPLY RULES ********************************************************************
applyRules=function(day,equity){
  cashin<-0
  cashout<-0
  return<-1

  candidates<-subset(stock,stock$date==day&stock$predictreturn>=returnthreshold&
                       stock$predictrpct<=rangethreshold)
  candidates<-candidates[order(-candidates$predictreturn),]
  numtrades<-nrow(candidates)
  if (numtrades>maxdaytrades) {
      candidates<-candidates[c(1:maxdaytrades),]
      numtrades<-maxdaytrades
  }
  if (numtrades>0) {
     candidates$buy<-NA
     candidates$sell<-NA
     candidates$return<-NA
     confidence_sum <- sum(candidates$predictrpct * candidates$predictreturn)
     candidates$confidence <- (candidates$predictrpct * candidates$predictreturn) / confidence_sum
     candidates <- candidates %>% 
       mutate(tradeweight = pmin(confidence * equity, maxtrade))
     
     for (i in 1:numtrades){
        candidates$buy[i]<-ifelse(candidates$tradetype[i]==1,candidates$open[i],candidates$close[i])
        candidates$sell[i]<-ifelse(candidates$tradetype[i]==1,candidates$close[i],candidates$open[i])
        candidates$return[i]<-1+(candidates$sell[i]-candidates$buy[i])/candidates$buy[i]
     }
   
     cashin<-sum(candidates$tradeweight)
     cashout<-sum(candidates$return*candidates$tradeweight)
     return <- 1+((cashout/equity)-(cashin/equity))
  } else candidates<-NULL
  return(list(trades=candidates,cashin=cashin,cashout=cashout,return=return))
}

# ****************************** CALCULATE PORTFOLIO STATISTICS ************************************************
portfolioStats=function(trades,preturn,tdays){
  tradedays<-length(unique(trades$date))
  totaldays<-length(tdays)
  pctdaystraded<-tradedays/totaldays
  totaltrades<-nrow(trades)
  shorttrades<-nrow(subset(trades,trades$tradetype==-1))
  longtrades<-nrow(subset(trades,trades$tradetype==1))
  cumreturn<-rep(1,length(totaldays))
  maxreturn<-cumreturn
  for (i in c(1:totaldays)){
    cumreturn[i]<-prod(preturn[c(1:i)],na.rm=TRUE)
    maxreturn[i]<-max(cumreturn[c(1:i)],na.rm=TRUE)
  }
  down<-cumreturn-maxreturn
  downpct<-down/maxreturn
  streak<-0
  maxstreak<-0
  for (i in c(1:totaldays)){
    streak<-ifelse(down[i]<0,streak+1,0)
    maxstreak<-ifelse(streak>maxstreak,streak,maxstreak)
  }
  plot(cumreturn,type="l",col="black",lwd=2,xlab="Time Period",ylim=c(min(cumreturn)-0.25,max(maxreturn)+0.25),
       ylab="Portfolio Return",main="Portfolio Results",xaxt = "n", yaxt = "n")
  lines(maxreturn,co=2,lw=2)
  lines(preturn,co=4,lw=2)
  axis(1, at = seq(0, totaldays+60, 30))
  axis(2, at = seq(0.5,max(maxreturn)+0.25, 0.1))
  cumreturn<-cumreturn[totaldays]
  meanreturn<-mean(preturn,na.rm=TRUE)-1
  sharpe<-meanreturn/sd(preturn,na.rm=TRUE)*sqrt(252)
  maxdraw<-min(downpct*100)
  
  # Compute number of winning long trades, winning percentage, and average return of long trades
  winlong <- nrow(subset(trades, trades$tradetype == 1 & trades$return > 1))
  winlongpct <- ifelse(longtrades > 0, winlong / longtrades, NA)
  avglongreturn <- ifelse(longtrades > 0, mean(subset(trades, trades$tradetype == 1)$return), NA)
  
  # Compute number of winning short trades, winning percentage, and average return of short trades
  winshort <- nrow(subset(trades, trades$tradetype == -1 & trades$return > 1))
  winshortpct <- ifelse(shorttrades > 0, winshort / shorttrades, NA)
  avgshortreturn <- ifelse(shorttrades > 0, mean(subset(trades, trades$tradetype == -1)$return), NA)
  
  # Compute percentage winning trades
  wintrades <- nrow(subset(trades, trades$return > 1))
  wintradespct <- ifelse(totaltrades > 0, wintrades / totaltrades, NA)
  performance<-list(totaltrades=totaltrades,
                    longtrades=longtrades,winlong=winlong, winlongpct = winlongpct, avglongreturn = avglongreturn,
                    shorttrades=shorttrades,winshort=winshort, winshortpct = winshortpct, avgshortreturn=avgshortreturn,
                    wintrades=wintrades, wintradespct =wintradespct,
                    cumreturn=cumreturn,meanreturn=meanreturn,
                    sharpe=sharpe,maxdraw=maxdraw,drawlength=maxstreak)
  return(performance)
}


# ********************************  RUN STRATEGY **********************************
stock<-NULL
for (sym in symbols) {
  temp<-genIndicators(sym,window)
  if (is.null(stock)) {
    stock<-temp} else
    stock<-rbind(stock,temp)
}

add<-read.csv("SP additions.csv")
add$date.added<-as.Date(add$date.added,format="%m/%d/%Y")
temp<-merge(stock,add,all.x=TRUE)
temp$date.added<-as.Date(ifelse(is.na(temp$date.added),as.Date("2000-12-31"),temp$date.added))
temp<-subset(temp,temp$date>=temp$date.added)

# Get rid of signals after stock is removed from the S&P 500

remove<-read.csv("SP removals.csv")
remove$date.removed<-as.Date(remove$date.removed,format="%m/%d/%Y")
temp<-merge(temp,remove,all.x=TRUE)
temp$date.removed<-as.Date(ifelse(is.na(temp$date.removed),as.Date("2100-12-31"),temp$date.removed))
temp<-subset(temp,temp$date<temp$date.removed)
temp<-temp[order(temp$symbol,temp$date),]
stock<-temp[,c(1:10)]

tdays<-unique(stock$date)
currentequity<-initialequity
trades<-NULL
preturn<-rep(length(tdays),1)
for (day in 1:length(tdays)) {
  date<-tdays[day]
  results<-applyRules(date,currentequity)
  if (is.null(trades)) {
    trades<-results$trades
  } else if (!is.null(results$trades)) {
      trades<-rbind(trades,results$trades)
  }
  currentequity<-currentequity-results$cashin+results$cashout
  preturn[day]<-results$return
}  
  
performance<-portfolioStats(trades,preturn,tdays) ; performance
