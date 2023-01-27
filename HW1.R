#HW 1 Answer 1
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
load('OHLC.RData')
sectors<-read_csv(file = 'sectors.csv')
symbolList <- stock[,1]
tradingDays <- as.data.frame(table(symbolList))
maximumTradingDays <- max(tradingDays$Freq)
print(maximumTradingDays)
symbolList <- tradingDays[tradingDays$Freq == maximumTradingDays,1]
length(symbolList)
ans1 <- data.frame(symbolList)
ans1$annualreturns = NA
colnames(ans1)[1] = "Symbol"
colnames(ans1)[2] = "AnnualReturn"
for(symbol in symbolList){
    open = head(stock[stock$symbol == symbol,3],1)
    close = tail(stock[stock$symbol == symbol,6],1)
    ans1$AnnualReturn[ans1$Symbol==symbol]<-round(((close-open)/open)*100,1)
  }
ans1<-ans1[order(ans1$AnnualReturn),]
print("Top 10 Stocks with Highest Annual Returns:")
tail(ans1,10)[seq(dim(tail(ans1,10))[1],1),]
print("Top 10 Stocks with Lowest Annual Returns:")
head(ans1,10)

#HW 1 Answer 2
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
load('OHLC.RData')
sectors<-read_csv(file = 'sectors.csv')
sectorsTemp <- data.frame(sectors)
sectorsTemp$annualreturns = NA
for(symbol in sectorsTemp[,1]){
  if(any(stock$symbol==symbol)){
  open = head(stock[stock$symbol == symbol,3],1)
  close = tail(stock[stock$symbol == symbol,6],1)
  sectorsTemp$annualreturns[sectorsTemp$symbol==symbol]<-round(((close-open)/open)*100,1)
  }
}
ans2 <- aggregate(sectorsTemp$annualreturns, list(sectorsTemp$sector),FUN=mean, na.rm = TRUE)
colnames(ans2)[1] = "Sector"
colnames(ans2)[2] = "AnnualReturn"
ans2$AnnualReturn <- round(ans2$AnnualReturn,1)
ans2

#HW 1 Answer 3 
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
load('OHLC.RData')
sectors<-read_csv(file = 'sectors.csv')
stocksMerged <- left_join(stock, sectors, by = c("symbol" = "symbol"))
stocksMerged$month <- month(stocksMerged$date)
stocksGroupbyMonth <- stocksMerged %>% 
  group_by(sector, month) %>%
    summarize(returns = ((last(close) - first(open)) / first(open))*100)
ans3 <- spread(stocksGroupbyMonth, month, returns)
ans3