library(IBrokers)
library(dplyr)
IBport=7496

getPortfolio=function(accountUpdate){
  currentPorfolio <- data.frame(matrix(ncol = 6, nrow = 0))
  for(i in c(1:length(accountUpdate[[2]])))
  {
    trade<-accountUpdate[[2]][[i]]
    sym<-trade[[1]]$symbol
    pos<-trade[[2]]$position
    price<-trade[[2]]$marketPrice
    value<-trade[[2]]$marketValue
    cost<-trade[[2]]$averageCost
    pnl<-trade[[2]]$unrealizedPNL
    if(pos!=0){
      new_row = c(symbol = sym, position=pos,marketPrice=price,marketValue=value,averageCost=cost,unrealizedPnL=pnl)
      currentPorfolio = rbind(currentPorfolio,new_row)
    }
  }
  colnames(currentPorfolio) <- c('symbol', 'position','marketPrice','marketValue','averageCost','unrealizedPnL')
  return(currentPorfolio)
}
getAccountDetails=function(accountUpdate){
  AvailableFunds <- accountUpdate[[1]]$AvailableFunds[1]
  AccruedCash <- accountUpdate[[1]]$AccruedCash[1]
  BuyingPower <- accountUpdate[[1]]$BuyingPower[1]
  CashBalance <- accountUpdate[[1]]$CashBalance[1]
  FullExcessLiquidity <- accountUpdate[[1]]$FullExcessLiquidity[1]
  FullInitMarginReq <- accountUpdate[[1]]$FullInitMarginReq[1]
  GrossPositionValue <- accountUpdate[[1]]$GrossPositionValue[1]
  UnrealizedPnL <- accountUpdate[[1]]$UnrealizedPnL[1]
  
  return(list(AvailableFunds=AvailableFunds,AccruedCash=AccruedCash,BuyingPower=BuyingPower,CashBalance=CashBalance,
              FullExcessLiquidity=FullExcessLiquidity,FullInitMarginReq=FullInitMarginReq,
              GrossPositionValue=GrossPositionValue,UnrealizedPnL=UnrealizedPnL))
}
tws <-tryCatch({                  
  tws = twsConnect(port=IBport)     # connection can fail with an error  
}, warning=function(w) {tws<-NULL }, error=function(e) {tws<-NULL})

if(!is.null(tws)) {
  accUpdate<-reqAccountUpdates(tws)  
  currentPortfolio<-getPortfolio(accUpdate)
  accountDetails<-getAccountDetails(accUpdate)
  print(accountDetails)
  print(currentPortfolio)
}
