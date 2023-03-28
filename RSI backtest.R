# ******************************************************************************
# RSI STRATEGY -> for a set of equities, over a trading period, invest in the 
# next market open those stocks for which RSI dips below a certain value and 
# close out all open positions for which  RSI rises above a certain value  
# **************SET WORKING DIRECTORY AND CLEAR ENVIRONMENT ********************
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())
options(scipen=999)
# ***************GET DATA AND SET TRADING DATE RANGE ***************************
library(quantmod)
library(dplyr)
library(TTR)
#Data with all the 5 year including the time the data was not art of S&P 5000
load("SPuniverseHW3.rdata")
from<-as.Date("2021-01-01")
to<-as.Date("2022-12-31")
load('sectors.rdata')
universe<-stock
symbolsUE <- subset(sectors,sectors$sector=='Energy')
#symbolsUE <- subset(sectors,sectors$sector=='Health Care')
#symbolsUE <- subset(sectors,sectors$sector=='Information Technology')
#symbolsUE <- subset(sectors,sectors$sector=='Financials')
#symbolsUE <- subset(sectors,sectors$sector=='Utilities')
#symbolsUE <- subset(sectors,sectors$sector=='Real Estate')
#symbolsUE <- subset(sectors,sectors$sector=='Materials')
#symbolsUE <- subset(sectors,sectors$sector=='Industrials')
symbols<- as.vector(symbolsUE$symbol)
numsymbols<-length(symbols)
CalcPeriod<-6 
universe<-subset(universe,universe$symbol %in% symbols
                 &universe$date>=from-(4*CalcPeriod)&universe$date<=to)
stock<-NULL
initialequity<-100000                 # starting money
maxdaytrades<-floor(numsymbols/2)                       # maximum trades in one day
maxtrade<-((initialequity*0.9)/maxdaytrades)*(1.5)                 # maximum value of any single trade
LowRSI<-22                          # buy below this value
HighRSI<-78                           # sell above this value
                      # RSI calculation period
# ****************************** GENERATE INDICATORS ***************************
# The indicator for this strategy is momentum.  We will also build bands around 
# the momentum by specifying a width that is predicated on the standard 
# deviation of momentum.  
genIndicators=function(symbol){
  print(paste('Generating Indicators for symbol: ',sym))
  stock<-subset(universe,universe$symbol==sym)                
  if (nrow(stock)>100) {
    stock.xts<-xts(stock[,c(3:7)],stock$date)                   
    stock.xts$rsi<-tryCatch({
      stock.xts$rsi<-RSI(stock.xts$close,n=CalcPeriod)        
    }, warning=function(w) {rsi<-NULL }, 
       error=function(e) {rsi<-NULL})
    macd <- MACD(stock.xts$close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
    stock.xts$macd <- macd[, "macd"]
    stock.xts$macd.signal <- macd[, "signal"]
    stock.xts$macd.direction <- ifelse(stock.xts$macd > stock.xts$macd.signal, 1, -1)
    stock<-data.frame(stock.xts)                                
    date<-as.Date(rownames(stock))                              
    stock<-cbind(sym,date,stock)                                    
    names(stock)[1]<-"symbol"
    rownames(stock)<-seq(1,nrow(stock),1)                       
  } else stock<-NULL
  return(stock)
}

# ************************************** GENERATE SIGNALS ***************************************************************
# For this strategy, we need two signals one for when RSI crosses above the 
# a threshold value (sell) and when it crosses below a different treshold value
# (buy).
genSignals=function(sym){
  print(paste('Generating Signals for symbol: ',sym))
  stock<-subset(indicators,indicators$symbol==sym)
  lagged.rsi<-stats::lag(stock$rsi)
  stock$cross.lt.value<-ifelse(lagged.rsi<=LowRSI,1,0)
  stock$cross.gt.value<-ifelse(lagged.rsi>=HighRSI,1,0)
  return(stock)
}

# **************************************** APPLY RULES ********************************************************************
# The rules in this case are "fairly" straightforward.  First, close out all 
# position for which we have # a sell signal (and actually hold a position).  
# Then buy all equities for which we have a buy signal, subject to the following
# constraints.  No more than MaxDayTrades positions can be opened in any one 
# period and we cannot invest more than MaxTrade $ in any equity and we will 
# invest as equally as possible into each equity... subject to an integer number
# of shares.  Since it is possible to have more buy signals than the constraint 
# MaxDayTrades, the order of selection is prioritized by the RSI value.
applyRules=function(currdate,equity){
  cashin<-0
  cashout<-0
  # First close out positions based on sell signals. 
  tempsell<-filter(signals, date == as.Date(currdate) & cross.gt.value==1)[,c(1,3)]
  if (nrow(tempsell)>0) {
    names(tempsell)[2]<-"sellprice"
    sell<-merge(position,tempsell)  # only sell to close out positions, no shorting
    if (nrow(sell)>0) {
      sell$liquidate<-sell$sellprice*sell$position
      sell$profit<-(sell$sellprice-sell$buyprice)*sell$position
      cashin<-sum(sell$liquidate)   # get the aggregate value to add back to currentcash
    } else sell<-NULL 
    } else sell<-NULL
  # now figure out what new positions to open based our signals and only if we don't already hold a position
  candidates<-filter(signals, date == as.Date(currdate) & cross.lt.value==1)
  if (!is.null(position)) { 
    if (nrow(position)>0) {
      dummy<-rep("pos",nrow(position))
      temp<-data.frame(position$symbol,dummy)
      rm(dummy)
      names(temp)[1]<-"symbol"
      candidates<-merge(candidates,temp,by="symbol", all.x=TRUE)
      candidates<-subset(candidates,is.na(candidates$dummy))
      if (nrow(candidates)>0) {
        candidates<-candidates[,-ncol(candidates)]
      }
    }
  }
  if (nrow(candidates)>0) {  # there are potential candidates 1 or more
    candidates<-candidates[order(-candidates$rsi),]  # sort them by decreasing momentum
    numtrades<-nrow(candidates)  
    if (numtrades>maxdaytrades) {                     # make sure we don't exceed the # of trades allowed
      candidates<-candidates[c(1:maxdaytrades),]
      numtrades<-maxdaytrades
    }
    tradeamount<-max(min(maxtrade,equity/numtrades),0) # invest equally, but not more than we have
    cashout<-0
    # now figure out how much cash we have used to invest in our new positions
    if (numtrades>0) {
      candidates$position<-NA
      candidates$buyprice<-candidates$open
      for (i in 1:numtrades){
        candidates$position[i]<-trunc(tradeamount/candidates$open[i])
        cashout<-cashout+candidates$position[i]*candidates$buyprice[i]
      }
      candidates<-subset(candidates,candidates$position>0)
    }
  } else candidates<-NULL
  return(list(buy=candidates,sell=sell,cashin=cashin,cashout=cashout))
}

# ****************************** CALCULATE PORTFOLIO STATISTICS ************************************************
# Calculate various portfolio statistics such as period returns, cumulative returns, number of trades,
# max drawdown period, max drawdown percent, and annualized sharpe ratio.  Much more can be done
# here.

portfolioStats=function(trades,pvalue,tdays){
  tradedays<-length(unique(trades$date))
  totaldays<-length(tdays)
  pctdaystraded<-tradedays/totaldays
  totaltrades<-nrow(trades)
  pdiff<-c(0,diff(pvalue))
  preturn<-pdiff/pvalue+1
  shorttrades<-0 #  shorttrades<-nrow(subset(trades,trades$tradetype==-1))
  longtrades<-totaltrades #  longtrades<-nrow(subset(trades,trades$tradetype==1))
  cumreturn<-rep(1,length(totaldays))
  maxvalue<-cumreturn
  maxreturn<-cumreturn
  for (i in c(1:totaldays)){
    cumreturn[i]<-prod(preturn[c(1:i)],na.rm=TRUE)
    maxreturn[i]<-max(cumreturn[c(1:i)],na.rm=TRUE)
    maxvalue[i]<-max(pvalue[c(1:i)],na.rm=TRUE)
  }
  down<-pvalue-maxvalue
  downpct<-(pvalue-maxvalue)/maxvalue
  streak<-0
  maxstreak<-0
  for (i in c(1:totaldays)){
    streak<-ifelse(down[i]<0,streak+1,0)
    maxstreak<-ifelse(streak>maxstreak,streak,maxstreak)
  }
  maxy<-max(cumreturn+0.2)
  plot(tdays,cumreturn,type="l",col="black",lwd=2,xlab="Time Period",ylim=c(0.5,maxy),ylab="Portfolio Return",main="Portfolio Results")
  lines(tdays,maxreturn,co=2,lw=2)
  lines(tdays,preturn,co=4,lw=2)
  # legend("topleft",co=c(1,2,4),lw=c(2,2,2),legend=c("Cum Return","Max Return","Daily Return"))
  
  cumreturn<-cumreturn[totaldays]
  meanreturn<-mean(preturn,na.rm=TRUE)-1
  sharpe<-meanreturn/sd(preturn,na.rm=TRUE)*sqrt(252)
  maxdraw<-min(down)
  maxdrawpct<-min(downpct)*100
  performance<-list(totaltrades=totaltrades,longtrades=longtrades,shorttrades=shorttrades,cumreturn=cumreturn,
                    meanreturn=meanreturn,sharpe=sharpe,maxdraw=maxdraw,maxdrawpct=maxdrawpct,drawlength=maxstreak)
  return(performance)
}


# ********************************  RUN STRATEGY **********************************
indicators<-NULL                               # we will put all OHLC data and our generated
for (sym in symbols) {                         # indicaters into a dataframe named "indicators"
  temp<-genIndicators(sym)                     # by looping through all the symbols in our
  if (!is.null(temp)) {                        # restricted universe. Need to ensure we have indicators  
    if (is.null(indicators)) {                   
      indicators<-temp} else
      indicators<-rbind(indicators,temp)
  }
  }
symbols<-unique(indicators$symbol)
signals<-NULL                                  # signals will be added to indicators and dumped   
for (sym in symbols) {                         # into a separate dataframe called "signals"
  temp<-genSignals(sym)
  if (is.null(signals)) {
    signals<-temp} else
    signals<-rbind(signals,temp)
}
signals<-na.omit(signals)
tdays<-unique(signals$date)                    # Now process (apply rules) for each trading day in
position<-NULL                                 # order... keeping track of open "positions" and
closed<-NULL                                   # "closed" positions as we proceed.  
pvalue<-rep(0,length(tdays))                   # Each day we will keep track of our portfolio value
currentcash<-initialequity                     # that includes current cash, plus our investments.
for (day in 1:length(tdays)) {                 # start of our loop
  currdate<-tdays[day]
  print(currdate)                              # simple update to screen on our progress
  results<-applyRules(currdate,currentcash)    # our state variables are the date and cash available
  if (is.null(results$sell)==FALSE) {          # if we sell stock, need to close out those positions  
    sell<-results$sell[,c(1,16:18)]            # grab the symbol and sell price, liquidate and profit
    temp<-merge(position,sell,by="symbol",all.x=TRUE)
    sell<-subset(temp,is.na(sellprice)==FALSE)# the prior merge links the open positions to the
    if (is.null(closed)==TRUE) {              # closed out stocks.  We will maintain a record of all closed positions
      closed<-sell }
    else closed<-rbind(closed,sell)
    position<-subset(temp,is.na(temp$sellprice)==TRUE)[,c(1:15)] # be sure not to include sell columns
    
  }
  if (is.null(position)) {                     # we will maintain a record of all open positions
    position<-results$buy
  } else if (is.null(results$buy)==FALSE) {
    position<-rbind(position,results$buy)
  }
  currentcash<-currentcash+results$cashin-results$cashout  # update our cash position at end of day
  if (!is.null(position)) {                                # update the value of our investments
    temp<-subset(indicators,indicators$date==currdate)[,c(1,6)]
    names(temp)[2]<-"currprice"
    currpos<-merge(position,temp)
    currpos$value<-currpos$position*currpos$currprice
    pvalue[day]<-sum(currpos$value,na.rm=TRUE)             # shouild not be missing values...
  } else pvalue[day]<-0
  pvalue[day]<-pvalue[day]+currentcash
}  

performance<-portfolioStats(closed,pvalue,tdays) ; performance



