# **************************************************************************
# Production platform for the Random Forest daily trading strategy.  
# Enter on open of next day and exit on open of day where reverse signal predicted
#Allows user to review/modify orders and then 
# either quit without trading or execute them through the API.  For the 
# API to work, the IB Gateway or Trader Workstation must be logged into and 
# active prior to running this script.

# TO EXECUTE PLEASE USE COMMAND ""source('HW6.R')"" on console.
# Other run calls can lead to code malfunction.

# *********SET WORKING DIRECTORY AND CLEAR ENVIRONMENT *********************
library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
rm(list=ls())
options("getSymbols.warning4.0"=FALSE) # disables warning message from showing 
options(scipen=999)
cat("\014")

# ************ SET DATE RANGES AND SYSTEM PARAMETERS ***************************
library(Quandl)
library(IBrokers)
library(quantmod)
library(dplyr)
library(tidyquant)
windowsize<-15
longestindicator<-200
currentdate<-Sys.Date()
maxdaytrades<-15                       # maximum trades in one day - one way (i.e. max 15 short, 15 long)
maxtradepct<-15               # maximum value of any single trade
longthreshold<-1.018
shortthreshold<-0.98
defaultscalinglength<-10000
currentSP500<-tq_index("SP500")[,c(1,6)]
IBport=7496 #7496 is the port for tws and 4002 is for the gate
# ************************* GET DATA FROM FROM STORED UNIVERSE AND IMPORT NEW FROM QUANDL *****************
getData=function(){
  stock<-NULL
  fromdate<-currentdate-2*(longestindicator+windowsize) 
  Quandl.api_key("EfNYF1EymebW8saMFp5B")
  temp<-NULL
  temp<-tryCatch({
    temp<-Quandl.datatable("SHARADAR/SEP", date.gte=fromdate,ticker="AAPL")   # Use tryCatch to handle the error  
  }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
  if(!is.null(temp)){
    #symbols<-subset(currentSP500,currentSP500$sector=='Energy')$symbol
    symbols<-currentSP500$symbol
    for (i in 1:length(symbols)) {
      print(c(i,symbols[i]))
      temp<-NULL
      temp<-tryCatch({
        temp<-Quandl.datatable("SHARADAR/SEP", date.gte=fromdate,ticker=symbols[i])   # Use tryCatch to handle the error
      }, warning=function(w) {temp<-NULL }, error=function(e) {temp<-NULL})
        stock<-rbind(stock,temp)
    }
    stock<-na.omit(stock)
    names(stock)[1]<-"symbol"
    stock<-stock[,c(1:7)]
    rownames(stock)<-seq(1,nrow(stock),1)
    days<-unique(stock$date)
    days<-days[order(days)]
    lastdate<-max(stock$date)
    datastart<-which(days==lastdate)-windowsize-longestindicator+1
    stock<-subset(stock,stock$date>=days[datastart])
  }
return(stock)  
}

# ************************* GENERATE INDICATORS *******************************
genIndicators=function(sym,tradedate){
  print(paste('Generating Indicators for symbol: ',sym))
  stock<-subset(universe,universe$symbol==sym) # work with one symbol at a time
  lastclose<-stock$close  #
  leadopen<-lead(as.vector(stock$open),1)
  leadclose<-lead(as.vector(stock$close),1)        
  savel<-data.frame(date=stock$date,nextopen=leadopen,nextclose=leadclose,lastclose=lastclose)
  stock.xts<-xts(stock[,c(3:7)],stock$date)                   
 
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
      scaled.xts<-scale(stock.xts[c(1:lengthforscaling),],center = mins, scale = maxs - mins)
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
          temp.xts<-scale(stock.xts[c(ws:i),],center = mins, scale = maxs - mins)           
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
    scaled.xts$cross1060<-stock.xts$cross1060
    scaled.xts$cross10100<-stock.xts$cross10100
    scaled.xts$rsi5<-stock.xts$rsi5
    scaled.xts$rsi10<-stock.xts$rsi10
    scaled.xts$rsi20<-stock.xts$rsi20
    stock<-data.frame(scaled.xts)                                
    date<-as.Date(rownames(stock))                              
    stock<-cbind(sym,date,stock)
    names(stock)[1]<-"symbol"
    stock<-merge(stock,currentSP500)
    stock$sector<-as.factor(stock$sector)
    stock$dow<-as.factor(weekdays(stock$date,abbreviate=TRUE))
    stock<-merge(stock,savel)
    stock<-stock[,c(2,1,3:ncol(stock))]
    stock$nextreturn<-(stock$nextclose-stock$nextopen)/stock$nextopen+1
    rownames(stock)<-seq(1,nrow(stock),1)
    } else stock<-NULL
  return(stock)
}

# ************************ GENERATE PREDICTIONS *******************************
genPredictions=function(stock,tradedate){
  library(ranger)
  results<-NULL
  print("training model...")
  train<-subset(stock,stock$date<as.Date(tradedate))
  train<-na.omit(train)
  rf.model=ranger(nextreturn~.-nextopen -nextclose -symbol -date ,data=train, 
                  mtry=18,num.trees=100)
  rsq<-round(mean(rf.model$r.squared),3)
  print(paste("RSQ:",rsq))
  preds<-subset(stock,stock$date==as.Date(tradedate))    
  preds$prediction<-predict(rf.model,preds)$predictions
  results<-preds
  colremove<-which(names(results)=="nextreturn")
  results<-results[,-colremove]
  colremove<-which(names(results)=="nextopen")
  results<-results[,-colremove]
  colremove<-which(names(results)=="nextclose")
  results<-results[,-colremove]
  return(results)
}

# **************************** GENERATE SIGNALS *******************************

genSignals=function(stock){
  stock$short<-ifelse(stock$prediction<shortthreshold,1,0)
  stock$long<-ifelse(stock$prediction>longthreshold,1,0)
  stock$price<-stock$lastclose
  stock<-subset(stock,stock$short==1|stock$long==1)
  return(list(stock))
}

# *************************** GENERATE TRADES *********************************

genTrades=function(candidates,equity){
  cashin<-0
  cashout<-0
  transcost<-0
  numtrades<-0
  maxtrade<-maxtradepct*equity
  if (nrow(candidates)>0) {  # there are potential candidates 1 or more
    candidates<-candidates[order(-candidates$prediction),]  # sort them by decreasing predicted returns
    numtrades<-nrow(candidates)  
    if (numtrades>maxdaytrades) { # make sure we don't exceed the # of trades allowed
      candidates<-candidates[c(1:maxdaytrades),]
      numtrades<-maxdaytrades
    }
    browser()
    candidates$tradeAmount<-NA
    candidates$tradeAmount <- min(equity * (candidates$prediction / sum(candidates$prediction)),maxtradepct*equity)
    cashout<-0
    # now figure out how much cash we have used to invest in our new positions
    if (numtrades>0) {
      candidates$position<-NA
      for (i in c(1:numtrades)){
        candidates$position[i]<-trunc(candidates$tradeAmount[i]/candidates$price[i])
        if (candidates$long[i]==1) {
            cashout<-cashout+candidates$position[i]*candidates$price[i]
        } else {
            cashin<-cashin+candidates$position[i]*candidates$price[i]
        }
      }
      candidates<-subset(candidates,candidates$position>0)
    }
  } else candidates<-NULL
  transcost<-1*numtrades+0.01*sum(candidates$position)
  return(list(trades=candidates,cashin=cashin,cashout=cashout,transcost=transcost))  
}


# ***************************** APPLY RULES ************************************
applyRules=function(currentPortfolio,day,equity){
  cashin<-0
  cashout<-0
  transcost<-0
  #browser()
  currentPortfolio$position <- as.numeric(currentPortfolio$position)
  #Extract Signals for Trades already Open
  currentPortfolioSignals <- signals[signals$symbol %in% currentPortfolio[,1],] 
  #Merge to add positions 
  currentPortfolioSignals <- merge(currentPortfolio, currentPortfolioSignals, by = "symbol")
  
  #Remove those signals from processing 
  signals <- signals[!(signals$symbol %in% currentPortfolio[, 1]), ] 

  candidates<-subset(signals,signals$date==day&signals$long==1)
  longs<-genTrades(candidates,3*equity/4)
  equity<-equity+longs$cashin-longs$cashout-longs$transcost
  candidates<-subset(signals,signals$date==day&signals$short==1)
  candidates$prediction<-2-candidates$prediction
  shorts<-genTrades(candidates,equity/4)
  
  #Remove buy for already long open positon
  currentPortfolioSignals <- currentPortfolioSignals[!(currentPortfolioSignals$long == 1 & currentPortfolioSignals$position > 0), ]
  #Remove sell for already short open positon
  currentPortfolioSignals <- currentPortfolioSignals[!(currentPortfolioSignals$short == 1 & currentPortfolioSignals$position < 0), ]
  
  #Filtering trades from current portfolio
  currentPortfolioTrades <- currentPortfolio[(currentPortfolio$symbol %in% currentPortfolioSignals[, 1]), ]
  
  #find current portfolio exit trades
  longcurrent <- subset(currentPortfolioTrades,currentPortfolioTrades$position<0)
  shortcurrent <- subset(currentPortfolioTrades,currentPortfolioTrades$position>0)
  
  #Combine current portfolio trades with new trades
  shorts[[1]] <- bind_rows(shorts[[1]],shortcurrent)
  longs[[1]] <- bind_rows(longs[[1]],longcurrent)
  
  cashin=longs$cashin+shorts$cashin
  cashout=longs$cashout+shorts$cashout+longs$transcost+shorts$transcost
  transcost=longs$transcost+shorts$transcost
  return(list(long=longs$trades,short=shorts$trades,cashin=cashin,
              cashout=cashout,transcost=transcost))
}

# *************************** REVIEW TRADES ************************************
reviewTrades=function(trades,tradetype) {
  done<-FALSE
  if (nrow(trades)>0){
    trades<-trades[,c("symbol","sector","prediction","price","position")]
    trades$prediction<-round(trades$prediction,digits=4)
    rownames(trades)<-seq(1,nrow(trades),1)
  } else {
    done<-TRUE
    choice<-"Q"
  }
  while (!done) {
    View(trades)
    cat("\014")
    print(paste("REVIEWING CANDIDATE",toupper(tradetype),
                " AND CHOOSING TO EXECUTE WILL SEND TRADES IMMEDIATELY TO IB."))
    choice<-readline(prompt="Choose D)elete trade, M)odify Price, C)hange position, E)xecute, Q)uit without trading:  ")
    choice<-toupper(choice)
    done<-ifelse(choice=="E"|choice=="Q",TRUE,FALSE)
    if (choice=="M"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to modify: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        newprice<-as.numeric(readline(prompt=paste("Enter the new limit price for",
                                                   trades$symbol[rownum],": ")))
        valid<-is.numeric(newprice)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          trades$price[rownum]<-newprice
        } else { 
            print("invalid price")  
            Sys.sleep(2)
        }
      } else {
          print("invalid row number") 
          Sys.sleep(2)
      }
    }
    if (choice=="C"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to change: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        newposition<-as.numeric(readline(prompt=paste("Enter the new position size for",trades$symbol[rownum],": ")))
        valid<-is.numeric(newposition)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          trades$position[rownum]<-newposition
        } else { 
          print("invalid position")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
        }
    }
    if (choice=="D"){
      rownum<-as.numeric(readline(prompt="Enter the row number corresponding to the trade you wish to delete: "))
      valid<-ifelse(rownum>=1&rownum<=nrow(trades),TRUE,FALSE)
      valid<-ifelse(is.na(valid),FALSE,valid)
      if (valid) {
        confirm<-as.character(readline(prompt=paste("Enter Y)es to confirm, N)o to abort removing the trade for ",trades$symbol[rownum],": ")))
        valid<-is.character(confirm)
        valid<-ifelse(is.na(valid),FALSE,valid)
        if (valid) {
          if (toupper(confirm)=="Y") {
            trades<-trades[-rownum,]
            if(nrow(trades)==0){
              print("Last candidate trade deleted")
              Sys.sleep(2)
            }
            }
        } else { 
          print("invalid entry")  
          Sys.sleep(2)
        }
      } else {
        print("invalid row number") 
        Sys.sleep(2)
      }
    }
    }
  if (choice!="E"){
    trades<-trades[-c(1:nrow(trades)),]
  }
  return(trades)
}

# ************************************** EXECUTE TRADES **********************************************************

executeTrades=function(trades,tradetype,tws){
if (isConnected(twsconn=tws)){
  print("Transmitting Orders...")
  openAction<-ifelse(toupper(tradetype)=="LONG","BUY","SELL")
  closeAction<-ifelse(toupper(tradetype)=="LONG","SELL","BUY")
  #Only opening positions as we carry forward the trades
  for(i in c(1:nrow(trades))){ # Enter orders to open positions
    equity<-twsEquity(as.character(trades$symbol[i]),'SMART',primary="ISLAND")
    OrdId<-reqIds(tws)
    order<-twsOrder(OrdId,action=openAction,totalQuantity = trades$position[i],
                    orderType="MKT",tif="OPG") 
    placeOrder(tws,equity,order)
    # cancelOrder(tws,OrdId)      
    }
  dummy<-readline(prompt="Orders submitted, press <Enter> to continue:")
  } else {
    print("Connection with Interactive Brokers lost.  Trades not submitted!")
    dummy<-readline(prompt="Press <Enter> to continue:")
    }
}

oktoProceed=function(lasttradedate){
  done<-FALSE
  proceed<-"N"
  message<-paste("Make sure that the IB Gateway is open.  Last data date is",
                 as.character(lasttradedate),"proceed (Y/N)?")
  #while (!done) {
   # cat("\014")
  proceed<-readline(prompt=message)
  proceed<-toupper(proceed)
  done<-ifelse(proceed=="Y"|proceed=="N",TRUE,FALSE)
#}
  return(proceed)
}

#Function to fetch current Portfolio Positions
getPortfolio=function(accountUpdate){
  currentPortfolio <- data.frame(matrix(ncol = 2, nrow = 0))
  for(i in c(1:length(acc[[2]])))
  {
    trade<-acc[[2]][[i]]
    sym<-trade[[1]]$symbol
    pos<-trade[[2]]$position
    if(pos!=0){
      new_row = c(symbol = sym, position=pos)
      currentPortfolio = rbind(currentPortfolio,new_row)
    }
  }
  colnames(currentPortfolio) <- c('symbol', 'position')
  return(currentPortfolio)
}
# ********************************  RUN STRATEGY **********************************
cat("\014")
universe<-getData()
lasttradedate<-max(as.Date(universe$date))
proceed<-oktoProceed(lasttradedate)
if (proceed=="Y"){
  symbols<-unique(universe$symbol)
  indicators<-NULL            # we will put all OHLC data and our generated
  for (sym in symbols) {      # indicators into a dataframe named "indicators"
    temp<-genIndicators(sym,lasttradedate)  # by looping through all the symbols 
    indicators<-rbind(indicators,temp)
    }
  predictions<-data.frame(genPredictions(indicators,lasttradedate))  
  signals<-data.frame(genSignals(predictions))
  signals<-na.omit(signals)
  

# ******************* OPEN CONNECTION & PROCESS TRADES  ************************

  tws <-tryCatch({                  # IBport is a global parameter   
  tws = twsConnect(port=IBport)     # connection can fail with an error  
  }, warning=function(w) {tws<-NULL }, error=function(e) {tws<-NULL})
  if(!is.null(tws)) {
    acc<-reqAccountUpdates(tws)  
    available<-max(as.numeric(acc[[1]]$AvailableFunds[1])/10,100000)
    currentPortfolio<-getPortfolio(acc)
    trades<-applyRules(currentPortfolio,lasttradedate,available)
    if (!is.null(trades$long)) {
      longs<-reviewTrades(trades[[1]],"Long Trades")
      if(nrow(longs)>0){
        executeTrades(longs,"Long",tws)
      } else {print("No long trades executed")}
    } else {print("No long trades to execute.")}
    Sys.sleep(5)
    if (!is.null(trades$short)) {
      shorts<-reviewTrades(trades[[2]],"Short Trades")
      if (nrow(shorts)>0) {
        executeTrades(shorts,"Short",tws)
      } else {print("No short trades executed")}
    } else {print("No short trades to execute.")}
  twsDisconnect(tws)
  } else {print("Aborting: Cannot connect to Interactive Brokers")}
}

