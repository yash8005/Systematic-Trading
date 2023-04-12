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
library(robustHD)
library(ranger)
load("SPuniverseHW3.rdata")
fromBacktest<-as.Date("2021-01-01")
toBacktest<-as.Date("2022-12-31")
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
universe<-subset(universe,universe$symbol %in% symbols)
CalcPeriod<-2
days<-unique(universe$date)
days<-days[order(days)]
yearsBacktest<-2
windowsize<-5                       # rolling training days for random forest
longestindicator<-200
teststart<-as.Date("2021-01-04")
datastart<-which(days==teststart)-windowsize-longestindicator
dataend<-which(days==teststart)+yearsBacktest*252+1-2
universe<-subset(universe,universe$date>=days[datastart]&universe$date<=days[dataend])
stock<-NULL
initialequity<-100000                # starting money
maxdaytrades<-floor(numsymbols/2)                       # maximum trades in one day
maxtrade<-((initialequity*0.9)/maxdaytrades)*(0.3)                 # maximum value of any single trade
defaultscalinglength<-10000
longthreshold<-1.005
shortthreshold<-0.985
trainstart<-datastart+windowsize+longestindicator
trainend<-dataend-1

# ************************************** GENERATE INDICATORS *******************
# The indicator for this strategy is momentum in RSI and MACD.  We will also build bands around 
# the momentum by specifying a width that is predicated on the standard 
# deviation of momentum.  
# ******************************************************************************
genIndicators=function(sym){
  print(paste('Generating Indicators for symbol: ',sym))
  stock<-unique(subset(universe,universe$symbol==sym))  
  if (nrow(stock)<100) {
    stock<-NULL
    return(stock)
  }
  stock.xts<-xts(stock[,c(3:7)],stock$date) 
  #print(stock.xts)
  
  #Rate of Change/Momentum
  
  stock.xts$momentum1<-diff(stock.xts$close)  
  stock.xts$accel<-diff(stock.xts$momentum1)
  stock.xts$momentum10<-tryCatch({
    stock.xts$momentum10<-momentum(stock.xts$close,n=10)    
  }, warning=function(w) {stock.xts$momentum10<-NA }, error=function(e) {stock.xts$momentum10<-NA})
  stock.xts$momentum20<-tryCatch({
    stock.xts$momentum20<-momentum(stock.xts$close,n=20)    
  }, warning=function(w) {stock.xts$momentum20<-NA }, error=function(e) {stock.xts$momentum20<-NA})
  
  #Oscillator
  
  macd<-tryCatch({
    macd<-MACD(stock.xts$close,maType="EMA")              
  }, warning=function(w) {macd<-NULL }, error=function(e) {macd<-NULL})
  if (is.null(macd)) {
    stock.xts$macdDiff<-NA
  } else stock.xts$macdDiff<-macd[,1]-macd[,2] 
  
  #Moving Averages
  
  stock.xts$sma5<-tryCatch({
    stock.xts$sma5<-SMA(stock.xts$close,n=5)        
  }, warning=function(w) {stock.xts$sma5<-NA }, error=function(e) {stock.xts$sma5<-NA})
  stock.xts$sma10<-tryCatch({
    stock.xts$sma10<-SMA(stock.xts$close,n=10)        
  }, warning=function(w) {stock.xts$sma10<-NA }, error=function(e) {stock.xts$sma10<-NA})
  stock.xts$sma20<-tryCatch({
    stock.xts$sma20<-SMA(stock.xts$close,n=20)        
  }, warning=function(w) {stock.xts$sma20<-NA }, error=function(e) {stock.xts$sma20<-NA})
  stock.xts$sma40<-tryCatch({
    stock.xts$sma40<-SMA(stock.xts$close,n=40)        
  }, warning=function(w) {stock.xts$sma40<-NA }, error=function(e) {stock.xts$sma40<-NA})
  
  stock.xts$cross510<-stock.xts$sma5/stock.xts$sma10
  stock.xts$cross520<-stock.xts$sma5/stock.xts$sma20
  stock.xts$cross540<-stock.xts$sma5/stock.xts$sma40
  stock.xts$cross1020<-stock.xts$sma10/stock.xts$sma20
  stock.xts$cross1040<-stock.xts$sma10/stock.xts$sma40  
  
  #RSI 
  
  stock.xts$rsi5<-tryCatch({
    stock.xts$rsi5<-RSI(stock.xts$close,n=5)        
  }, warning=function(w) {stock.xts$rsi5<-NA }, error=function(e) {stock.xts$rsi5<-NA})
  stock.xts$rsi10<-tryCatch({
    stock.xts$rsi10<-RSI(stock.xts$close,n=10)      
  }, warning=function(w) {stock.xts$rsi10<-NA }, error=function(e) {stock.xts$rsi10<-NA})
  stock.xts$rsi20<-tryCatch({
    stock.xts$rsi20<-RSI(stock.xts$close,n=20)        
  }, warning=function(w) {stock.xts$rsi20<-NA }, error=function(e) {stock.xts$rsi20<-NA})
  
  # New Features
  
  #Volatility
  stock.xts$volatilityGK<-tryCatch({
    stock.xts$volatilityGK<-volatility(stock.xts, calc="garman")       
  }, warning=function(w) {stock.xts$volatilityGK<-NA }, error=function(e) {stock.xts$volatilityGK<-NA})
  
  stock.xts$volatilityParkinson<-tryCatch({
    stock.xts$volatilityParkinson<-volatility(stock.xts, calc="parkinson")       
  }, warning=function(w) {stock.xts$volatilityParkinson<-NA }, error=function(e) {stock.xts$volatilityParkinson<-NA})
  
  stock.xts$atr<-tryCatch({
    stock.xts$atr<-ATR(subset(stock.xts, select = c("high","low", "close")), n = 14)[,2]     
  }, warning=function(w) {stock.xts$atr<-NA }, error=function(e) {stock.xts$atr<-NA})
  #print(ATR(subset(stock.xts, select = c("high","low", "close")),n=14))
  stock.xts$chaikinVolatility<-tryCatch({
    stock.xts$chaikinVolatility<-chaikinVolatility(stock.xts, n = 10)     
  }, warning=function(w) {stock.xts$chaikinVolatility<-NA }, error=function(e) {stock.xts$chaikinVolatility<-NA})

  # trend direction/strength ------
  
  # CCI(HLC, n = 20, maType, c = 0.015, ...): Commodity Channel Index
  stock.xts$cci<-tryCatch({
    stock.xts$cci<-CCI(subset(stock.xts, select = c("high","low", "close")), n = 14)    
  }, warning=function(w) {stock.xts$cci<-NA }, error=function(e) {stock.xts$cci<-NA})
  
  # ADX(HLC, n = 14, maType, ...): Welles Wilder’s Directional Movement Index
  stock.xts$adx<-tryCatch({
    stock.xts$adx<-ADX(subset(stock.xts, select = c("high","low", "close")), n = 14)[,4]    
  }, warning=function(w) {stock.xts$adx<-NA }, error=function(e) {stock.xts$adx<-NA})
  
  # measure of the money flowing into or out of a security ------
  # OBV(price, volume): On Balance Volume
  stock.xts$obv<-tryCatch({
    stock.xts$obv<-OBV(stock.xts$close,stock.xts$volume)
  }, warning=function(w) {stock.xts$obv<-NA }, error=function(e) {stock.xts$obv<-NA})
  
  # CLV(HLC): Close Location Value
  stock.xts$clv<-tryCatch({
    stock.xts$clv<-CLV(subset(stock.xts, select = c("high","low", "close")))
  }, warning=function(w) {stock.xts$clv<-NA }, error=function(e) {stock.xts$clv<-NA})
  
  #WilliamAD
  stock.xts$williamsAD<-tryCatch({
    stock.xts$williamsAD<-williamsAD(subset(stock.xts, select = c("high","low", "close")))
  }, warning=function(w) {stock.xts$williamsAD<-NA }, error=function(e) {stock.xts$williamsAD<-NA})
  
  
  # Stochastic Oscillator / Momentum Index: ------
 
  # SMI(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9, maType, bounded = TRUE, ...): Stochastic Momentum Index
  smi<-tryCatch({
    smi<-SMI(subset(stock.xts, select = c("high","low", "close")))
  }, warning=function(w) {smi<-NA }, error=function(e) {smi<-NA})
  stock.xts$smi <- smi[,1]
  stock.xts$smi_signal <- smi[,2]
  
  # WPR(HLC, n = 14): William's %R
  stock.xts$wpr<-tryCatch({
    stock.xts$wpr<-WPR(subset(stock.xts, select = c("high","low", "close")))
  }, warning=function(w) {stock.xts$wpr<-NA }, error=function(e) {stock.xts$wpr<-NA})
  
 
  stock.xts<-na.omit(stock.xts)

  scaled.xts<-NULL
  lengthforscaling<-min(defaultscalinglength,nrow(stock.xts))
  if (lengthforscaling>0) {
    maxs <- apply(stock.xts[c(1:lengthforscaling),], 2, max)
    mins <- apply(stock.xts[c(1:lengthforscaling),], 2, min)
    scaled.xts<-tryCatch({
      scaled.xts<-scale(stock.xts[c(1:lengthforscaling),],center = mins, scale = maxs - mins)        # sometimes calls to TTR functions will crash the system   
    }, warning=function(w) {scaled.xts<-NULL }, error=function(e) {scaled.xts<-NULL})
  }

  if (!is.null(scaled.xts)){ 
    if (lengthforscaling<nrow(stock.xts$close)) {
      startscale<-lengthforscaling+1
      stopscale<-nrow(stock.xts)
      for (i in c(startscale:stopscale)){
        ws<-i-lengthforscaling+1
        maxs <- apply(stock.xts[c(ws:i),], 2, max)
        mins <- apply(stock.xts[c(ws:i),], 2, min)
        temp.xts<-tryCatch({
          temp.xts<-scale(stock.xts[c(ws:i),],center = mins, scale = maxs - mins)        # sometimes calls to TTR functions will crash the system   
        }, warning=function(w) {temp.xts<-NULL }, error=function(e) {temp.xts<-NULL})
        nextrow<-nrow(temp.xts)
        scaled.xts<-rbind(scaled.xts,temp.xts[nextrow,])
      }
    }
    scaled.xts$cross510<-stock.xts$cross510
    scaled.xts$cross520<-stock.xts$cross520
    scaled.xts$cross540<-stock.xts$cross540
    scaled.xts$cross1020<-stock.xts$cross1020
    scaled.xts$cross1040<-stock.xts$cross1040
    scaled.xts$rsi5<-stock.xts$rsi5
    scaled.xts$rsi10<-stock.xts$rsi10
    scaled.xts$rsi20<-stock.xts$rsi20
   
    scaled.xts$nextreturn<-(lead(as.vector(stock.xts$close),1)-lead(as.vector(stock.xts$open),1))/lead(as.vector(stock.xts$open),1)+1
    scaled.xts$nextopen<-lead(as.vector(stock.xts$open),1)
    scaled.xts$nextclose<-lead(as.vector(stock.xts$close),1)
   
    stock<-data.frame(scaled.xts)      # convert back to dataframe object
    
    date<-as.Date(rownames(stock))                              
    stock<-cbind(sym,date,stock)
    names(stock)[1]<-"symbol"
  
    
    stock$dow<-as.factor(weekdays(stock$date,abbreviate=TRUE))
    for (i in (1:ncol(stock))){
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

genPredictions=function(stock){
  results<-NULL
  for (currday in c(trainstart:(trainend))) {
    print(paste("processing day",as.Date(days[currday])))
    from<-days[currday-windowsize]
    to<-days[currday-1]
    train<-subset(stock,stock$date>=from&stock$date<=to)[3:ncol(stock)]
    #print(train)
    rf.model=ranger(nextreturn~.-nextopen -nextclose,data=train, mtry = 18, num.trees = 2000)
    rsq<-round(mean(rf.model$r.squared),3)
    print(paste("RSQ:",rsq))
    preds<-subset(stock,stock$date==as.Date(days[currday]))    
    preds$rsq<-rep(rsq,nrow(preds))
    preds$prediction<-predict(rf.model,preds)$predictions
    if (is.null(results)){
      results<-preds} else results<-rbind(results,preds)
  }
  return(results)
}

# ************************************** GENERATE SIGNALS ***************************************************************
# For this strategy, we need two signals one for when RSI crosses above the 
# a threshold value (sell) and when it crosses below a different treshold value
# (buy) and one for upward trend and downward trend. We have already calculated MACD and its direction.
# ******************************************************************************
genSignals=function(stock){
  stock$short<-ifelse(stock$prediction<shortthreshold,1,0)
  stock$long<-ifelse(stock$prediction>longthreshold,1,0)
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
                         (signals$short==1))[,c(1,2,38)] # grab symbol (1), date(2), and price (6)
    names(candidates)[2]<-"closedate"                        # keep track of the close date so we
    names(candidates)[3]<-"outprice"                         # can check how long we hold our positions
    closeshort<-merge(shortposition,candidates,by="symbol")  # Close only if we have a position
    candidates<-subset(signals,signals$date==day&            # Now do the same for longs
                         (signals$long==1))[,c(1,2,38)]    
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
                         (signals$short==1))
    temp<-merge(candidates,shortposition,by="symbol",all.x=TRUE)
    openshort<-subset(temp,is.na(dummy))                         # only short if we don't have a position
    if (nrow(openshort)>0) {                                      
      openshort<-openshort[,c(1:ncol(openshort)-1)]              # get rid of dummy column
      openshort$type<-"Short"                                    # we will open a short position
    } else {openshort<-NULL}                                     # if the dataframe is empty, set it to null
    candidates<-subset(signals,signals$date==day&                # now proceed and do same for longs
                         (signals$long==1))
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
             (short == 1  |
               long == 1 ) ) # no open positions so grab all signals to open

    if (nrow(opened)==0) {opened<-NULL} else {                    
      opened <- opened %>% 
        mutate(type = ifelse(opened$short == 1, "Short", "Long"))               # set the type of position (long, short)  
    }
  }

  if (!is.null(opened)) {                                        # open if we have positions to open
    opened$buyprice<-ifelse(opened$type=="Long",opened$nextopen,NA)
    opened$sellprice<-ifelse(opened$type=="Short",opened$nextopen,NA)

    #opened<-opened[order(-opened$rsi),]                         # sort them by the risk 
    numtrades<-nrow(opened)                                      # we will take the best maxtrades to    
    if (numtrades>maxdaytrades) {                                # open - we will not exceed maxtrades
      opened<-opened[c(1:maxdaytrades),]
      numtrades<-maxdaytrades
    }
    tradeamount<-max(min(maxtrade,equity/numtrades),0)           # invest equally, but not more than we have
    if (numtrades>0&tradeamount>0) {
      opened$position<-ifelse(opened$type=="Long",               # keep a record of the opening price
                              trunc(tradeamount/opened$nextopen),    # and the size of the position, negative
                              -trunc(tradeamount/opened$nextopen))   # position for shorts
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
  
  maxy<-max(cumreturn+0.2)
  miny<-min(cumreturn-0.01)
  plot(tdays,cumreturn,type="l",col="black",lwd=2,xlab="Time Period",ylim=c(miny,maxy),ylab="Portfolio Return",main="Portfolio Results")
  lines(tdays,maxreturn,co=2,lw=2)
  lines(tdays,preturn,co=4,lw=2)
  
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
                            mutate(return = (sellprice - buyprice) / buyprice) %>%
                            summarise(avg_return = mean(return)) %>% 
                              pull(avg_return)), NA)
  
  # Compute number of winning short trades, winning percentage, and average return of short trades
  winshort <- nrow(subset(trades, trades$type == "Short" & trades$profit <= 0))
  winshortpct <- ifelse(shorttrades > 0, winshort / shorttrades, NA)
  avgshortreturn <- ifelse(shorttrades > 0, (trades %>%
                                               filter(type == "Short") %>%
                                               mutate(return = ((sellprice) - (buyprice)) / (buyprice)) %>%
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
    
signals<-NULL    # signals will be added to indicators and dumped   
predictions<-data.frame(genPredictions(indicators))

signals<-data.frame(genSignals(predictions))
signals<-na.omit(signals)

signals<-subset(signals,signals$date>=fromBacktest&signals$date<=toBacktest)
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
