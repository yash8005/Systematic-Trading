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
library(readr)
library(zoo)
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
CalcPeriod<-2
universe<-subset(universe,universe$symbol %in% symbols
                 &universe$date>=from-(10*CalcPeriod)&universe$date<=to)
stock<-NULL
initialequity<-100000                # starting money
maxdaytrades<-floor(numsymbols/2)                       # maximum trades in one day
maxtrade<-((initialequity*0.9)/maxdaytrades)*(0.5)                 # maximum value of any single trade
LowRSI<-40                          # buy below this value
HighRSI<-80                           # sell above this value

# ************************************** GENERATE INDICATORS *******************
# The indicator for this strategy is momentum in RSI and MACD.  We will also build bands around 
# the momentum by specifying a width that is predicated on the standard 
# deviation of momentum.  
# ******************************************************************************
genIndicators=function(sym){
  print(paste('Generating Indicators for symbol: ',sym))
  stock<-subset(universe,universe$symbol==sym)  # work with one symbol at a time
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
# (buy) and one for upward trend and downward trend. We have already calculated MACD and its direction.
# ******************************************************************************
genSignals=function(sym){
  print(paste('Generating Signals for symbol: ',sym))
  stock<-subset(indicators,indicators$symbol==sym)
  stock$lagged.rsi<-lag(stock$rsi,1)
  stock$doublelagged.rsi <- lag(stock$rsi,2)
  stock$cross.lt.value<-ifelse(stock$lagged.rsi<=LowRSI,1,0)
  stock$cross.gt.value<-ifelse(stock$lagged.rsi>=HighRSI,1,0)
  stock$trend.down<-ifelse(stock$doublelagged.rsi < stock$lagged.rsi, 1, 0) # downtrend signal
  stock$trend.up<-ifelse(stock$doublelagged.rsi > stock$lagged.rsi, 1, 0) # uptrend signal  
  #stock<-data.frame(stock.xts)                                     
  return(stock)
}

# **************************CLOSE POSITIONS ************************************
# Here we will check our exit signals and compare them to the list of open
# positions, separately for longs and shorts.  If we have a short position
# and RSI is above the lower RSI and -ve MACD direction, then we close it.   If we have a long
# position and RSI is above higher RSI and +ve MACD direction, then we close it.  Note we
# will note simultaneously hold long and short positions with this strategy.
# We will only open if we don't already have an open position in a stock.  
# ******************************************************************************
closePositions=function(day,equity,position){
  cash<-0
  closed<-NULL
  if (!is.null(position)) {
    longposition<-subset(position,type=="Long")              # check long and short separately
    shortposition<-subset(position,type=="Short")           
    candidates<-subset(signals,signals$date==day&            # check shorts first
                         (signals$trend.down==1)&(signals$cross.gt.value==1)&(signals$macd.direction==-1))[,c(1,2,6)] # grab symbol (1), date(2), and nextopen (38)
    names(candidates)[2]<-"closedate"                        # keep track of the close date so we
    names(candidates)[3]<-"outprice"                         # can check how long we hold our positions
    closeshort<-merge(shortposition,candidates,by="symbol")  # Close only if we have a position
    candidates<-subset(signals,signals$date==day&            # Now do the same for longs
                         (signals$trend.up==1)&(signals$cross.lt.value==1)&(signals$macd.direction==1))[,c(1,2,6)]    
    names(candidates)[2]<-"closedate"
    names(candidates)[3]<-"outprice"
    closelong<-merge(longposition,candidates,by="symbol")
    closed<-rbind(closeshort,closelong)                      # put all our positions to close together
    if (nrow(closed)>0) {
      closed$closecash<-closed$outprice*closed$position      # compute closing calculations of cash
      closed$sellprice<-ifelse(closed$type=="Long",closed$outprice,closed$sellprice)
      closed$buyprice<-ifelse(closed$type=="Short",closed$outprice,closed$buyprice)
      closed$profit<-(closed$sellprice-closed$buyprice)*abs(closed$position)
      cash<-sum(closed$closecash)   # get the aggregate value to add back to currentcash
    } else closed<-NULL
  }
  return(list(closed=closed,cashin=cash))  
}

# ************************** OPEN POSITIONS ************************************
# Now we are going to check our entry signals and only enter a position if we
# don't already have a position in the stock.  So we have a signal to open, we
# need to check for the absence of the position in the set of open positions
# ******************************************************************************
#day<-currdate
#position<-netopen
openPositions=function(day,equity,position){
  cash=0
  opened<-NULL
  if (!is.null(position)) {                                      # only need to check if we have open positions
    longposition<-subset(position,type=="Long")[,c(1,2)]         # check long and shorts separately
    names(longposition)[2]<-"dummy"                              # use dummy again, see Apply Rules function
    shortposition<-subset(position,type=="Short")[,c(1,2)]       # for further explanation
    names(shortposition)[2]<-"dummy"
    candidates<-subset(signals,signals$date==day&                # check shorts first  
                         (signals$trend.down==1)&(signals$cross.gt.value==1)&(signals$macd.direction==-1))
    temp<-merge(candidates,shortposition,by="symbol",all.x=TRUE)
    openshort<-subset(temp,is.na(dummy))                         # only short if we don't have a position
    if (nrow(openshort)>0) {                                      
      openshort<-openshort[,c(1:ncol(openshort)-1)]              # get rid of dummy column
      openshort$type<-"Short"                                    # we will open a short position
    } else {openshort<-NULL}                                     # if the dataframe is empty, set it to null
    candidates<-subset(signals,signals$date==day&                # now proceed and do same for longs
                         (signals$trend.up==1)&(signals$cross.lt.value==1)&(signals$macd.direction==1))
    temp<-merge(candidates,longposition,by="symbol",all.x=TRUE)
    openlong<-subset(temp,is.na(dummy))
    if (nrow(openlong)>0) {
      openlong<-openlong[,c(1:ncol(openlong)-1)]
      openlong$type<-"Long" 
    } else {openlong<-NULL}
    opened<-rbind(openlong,openshort)                            # put all positions to be opened together
    if (!is.null(opened)) {                                      # convert empty dataframe to null
      if (nrow(opened)==0) opened<-NULL                          # so we don't have to check for both !null
    }                                                            # and that the number of rows>0
  } else {
    opened<-signals %>%
      filter(date == as.Date(day),
             (trend.down == 1 & cross.gt.value == 1 & macd.direction == -1) |
               (trend.up == 1 & cross.lt.value == 1 & macd.direction == 1))  # no open positions so grab all signals to open

    if (nrow(opened)==0) {opened<-NULL} else {                    
      opened <- opened %>% 
        mutate(type = ifelse(opened$trend.down == 1 & opened$cross.gt.value == 1 & opened$macd.direction==-1, "Short", "Long"))               # set the type of position (long, short)  
    }
  }

  if (!is.null(opened)) {                                        # open if we have positions to open
    opened$buyprice<-ifelse(opened$type=="Long",opened$open,NA)
    opened$sellprice<-ifelse(opened$type=="Short",opened$open,NA)

    opened<-opened[order(-opened$rsi),]                         # sort them by the risk 
    numtrades<-nrow(opened)                                      # we will take the best maxtrades to    
    if (numtrades>maxdaytrades) {                                # open - we will not exceed maxtrades
      opened<-opened[c(1:maxdaytrades),]
      numtrades<-maxdaytrades
    }
    tradeamount<-max(min(maxtrade,equity/numtrades),0)           # invest equally, but not more than we have
    if (numtrades>0&tradeamount>0) {
      opened$position<-ifelse(opened$type=="Long",               # keep a record of the opening price
                              trunc(tradeamount/opened$open),    # and the size of the position, negative
                              -trunc(tradeamount/opened$open))   # position for shorts
      opened$opencash<-ifelse(opened$type=="Long",               # update our cash position
                              opened$buyprice*opened$position,0)
      opened$opencash<-ifelse(opened$type=="Short",
                              opened$sellprice*opened$position,opened$opencash)
      opened<-subset(opened,opened$position!=0)
      cash<-sum(opened$opencash) 
    } else {opened<-NULL}
  } 
  return(list(opened=opened,cashout=cash))  
}


# **************************************** APPLY RULES *************************
# Apply rules will first check the signals and apply rules for closing out 
# positions then open any new positions.  We  won't add to existing positions so
# we will only open if we don't already have an open position in a stock.  
# ******************************************************************************
#results<-applyRules(currdate,currentcash,position)    # our state variables are the date and cash available
#equity<-currentcash

applyRules=function(currdate,equity,position){
  netopen<-position                                        # netopen will hold all open positions after any close orders
  close.results<-closePositions(currdate,equity,position)  # close any orders for which we have positions and signals
  if (!is.null(close.results$closed)) {                    # Did we actually close out any positions
    temp<-close.results$close[,c(1,2)]                     # if we we need to remove them from our open positions
    names(temp)[2]<-"dummy"                                # we need one field to check if it is empty after the merge
    temp<-merge(position,temp,by="symbol",all.x=TRUE)      # and we don't want to generate duplicate columns, hence dummy
    netopen<-subset(temp,is.na(temp$dummy))                # so if dummy is NA, then the position is not closed
    netopen<-netopen[,c(1:ncol(netopen)-1)]                # get rid of the dummy column
    equity<-equity+close.results$cashin                    # update our equity position with the cash from closing
  }
  open.results<-openPositions(currdate,equity,netopen)     # now check for opening new positions
  return(list(open=open.results$opened,close=close.results$closed,
         posnetofcloses=netopen,cashin=close.results$cash,cashout=open.results$cash))
}
# ************************** CALCULATE PORTFOLIO STATISTICS ********************
# Calculate various portfolio statistics such as period returns, cumulative 
# returns, number of trades, max drawdown period, max drawdown percent, and 
# annualized sharpe ratio.  The only change from what we have seen before
# with backtestings is that there is a new calculation to compute the mean
# number of days that we hold a position.  This provides an indicate of trading
# frequency.
# ******************************************************************************
portfolioStats=function(trades,pvalue,tdays){
  tradedays<-length(unique(trades$date))
  totaldays<-length(tdays)
  pctdaystraded<-tradedays/totaldays
  totaltrades<-nrow(trades)
  pdiff<-c(0,diff(pvalue))
  preturn<-pdiff/pvalue+1
  shorttrades<-nrow(subset(trades,type=="Short"))
  longtrades<-totaltrades-shorttrades
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
  
  plot(cumreturn,type="l",col="black",lwd=2,xlab="Time Period",ylim=c(min(cumreturn)-0.25,max(maxreturn)+0.25),
       ylab="Portfolio Return",main="Portfolio Results",xaxt = "n", yaxt = "n")
  lines(maxreturn,co=2,lw=2)
  lines(preturn,co=4,lw=2)
  axis(1, at = seq(0, totaldays+60, 30))
  axis(2, at = seq(0.5,max(maxreturn)+0.25, 0.1))
  
  trades$holdperiod<-as.numeric(trades$closedate-trades$date)
  meanhold<-mean(trades$holdperiod,na.rm=TRUE)
  cumreturn<-cumreturn[totaldays]
  meanreturn<-mean(preturn,na.rm=TRUE)
  sharpe<-(meanreturn-1)/sd(preturn,na.rm=TRUE)*sqrt(252)
  maxdraw<-min(down)
  maxdrawpct<-min(downpct)*100
  
  # Compute number of winning long trades, winning percentage, and average return of long trades
  winlong <- nrow(subset(trades, trades$type == "Long" & trades$profit > 0))
  winlongpct <- ifelse(longtrades > 0, winlong / longtrades, NA)
  avglongreturn <- ifelse(longtrades > 0, (trades %>%
                            filter(type == "Long") %>%
                            mutate(return = (close - open) / open) %>%
                            summarise(avg_return = mean(return)) %>% 
                              pull(avg_return)), NA)
  
  # Compute number of winning short trades, winning percentage, and average return of short trades
  winshort <- nrow(subset(trades, trades$type == "Short" & trades$profit <= 0))
  winshortpct <- ifelse(shorttrades > 0, winshort / shorttrades, NA)
  avgshortreturn <- ifelse(shorttrades > 0, (trades %>%
                                               filter(type == "Short") %>%
                                               mutate(return = ((close) - (open)) / (open)) %>%
                                               summarise(avg_return = mean(return))%>% 
                                               pull(avg_return)), NA)
  
  # Compute percentage winning trades
  wintrades <- nrow(subset(trades, trades$profit > 0))
  wintradespct <- ifelse(totaltrades > 0, wintrades / totaltrades, NA)
  performance<-list(totaltrades=totaltrades,longtrades=longtrades,winlong=winlong, winlongpct = winlongpct, avglongreturn = avglongreturn,
                    shorttrades=shorttrades,winshort=winshort, winshortpct = winshortpct, avgshortreturn=avgshortreturn,
                    wintrades=wintrades, wintradespct =wintradespct,cumreturn=cumreturn,
                    meanreturn=meanreturn,sharpe=sharpe,maxdraw=maxdraw,maxdrawpct=maxdrawpct,drawlength=maxstreak,
                    meanhold=meanhold)
  return(performance)
}


# ********************************  RUN STRATEGY **********************************
indicators<-NULL                               # we will put all OHLC data and our generated
for (sym in symbols) {                         # indicators into a dataframe named "indicators"
  temp<-genIndicators(sym)                     # by looping through all the symbols in our
  indicators<-rbind(indicators,temp)
}
    
signals<-NULL                                  # signals will be added to indicators and dumped   
for (sym in symbols) {                         # into a separate dataframe called "signals"
  temp<-genSignals(sym)
  signals<-rbind(signals,temp)
}
signals<-subset(signals,signals$date>=from&signals$date<=to)
tdays<-unique(signals$date)                             # Now process (apply rules) for each trading day in
position<-NULL                                          # order... keeping track of open "positions" and
closed<-NULL                                            # "closed" positions as we proceed.  
pvalue<-rep(0,length(tdays))                            # Each day we will keep track of our portfolio value
currentcash<-initialequity                              # that includes current cash, plus our investments.
for (day in 1:length(tdays)) {                          # Now backtest throughout the trading period
  currdate<-tdays[day]
  print(currdate)                                       # simple update to screen on our progress
  results<-applyRules(currdate,currentcash,position)    # our state variables are the date and cash available
  position<-rbind(results$posnetofcloses,results$open)  # open positions - what we didn't close+ new positions
  closed<-rbind(closed,results$close)                   # keep track of all our closed positions
  currentcash<-currentcash+results$cashin-results$cashout  # update our cash position at end of day
  if (!is.null(position)) {                                # update the value of our investments
    temp<-subset(indicators,indicators$date==currdate)[,c(1,6)]
    names(temp)[2]<-"currprice"
    currpos<-merge(position,temp)
    currpos$value<-currpos$position*currpos$currprice
    pvalue[day]<-sum(currpos$value,na.rm=TRUE)           # should not be missing values...
  } else pvalue[day]<-0
  pvalue[day]<-pvalue[day]+currentcash
}  
  
performance<-portfolioStats(closed,pvalue,tdays) 
performance



