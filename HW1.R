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

#HW 1 Answer 4
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
load('OHLC.RData')
sectors<-read_csv(file = 'sectors.csv')
appleData <- stock[stock$symbol=='AAPL',]
appleData$dailyreturn <- ((appleData$close - appleData$open) / appleData$open)+1
appleData$cumulativereturn <- cumprod(appleData$dailyreturn)
appleData$maxcumulativereturn = cummax(appleData$cumulativereturn)

p1 <- ggplot(appleData, aes(x = date, y = dailyreturn)) +
  geom_line(color="orange") +
  ggtitle("Daily Return")+
  theme_economist() +
  scale_y_continuous(limits = c(min(appleData$dailyreturn)-0.02, max(appleData$dailyreturn)+0.02), expand = c(0, 0))+
  xlab("Date") +
  ylab("Return") 

p2 <- ggplot(appleData, aes(x = date, y = cumulativereturn)) +
  geom_line(color="darkgreen") +
  ggtitle("Cumulative Return") +
  theme_economist() +
  scale_y_continuous(limits = c(min(appleData$cumulativereturn)-0.1, max(appleData$cumulativereturn)+0.1), expand = c(0, 0))+
  xlab("Date") +
  ylab("Return") 

p3 <- ggplot(appleData, aes(x = date, y = maxcumulativereturn)) +
  geom_line(color="blue") +
  ggtitle("Max Cumulative Return") +
  theme(panel.grid.major = element_line(color = "gray", size = 0.5),
                  panel.grid.minor = element_line(color = "gray", size = 0.25)) +
  theme_economist()+
  scale_y_continuous(limits = c(min(appleData$maxcumulativereturn)-0.1, max(appleData$maxcumulativereturn)+0.1), expand = c(0, 0))+
  xlab("Date") +
  ylab("Return") 


# Create a line plot with multiple lines for each type of return
p4 <- ggplot(appleData, aes(x = date)) +
  geom_line(aes(y = cumulativereturn, group = 1, color = "Cumulative Return")) +
  geom_line(aes(y = dailyreturn, group = 1, color = "Daily Return"), linewidth = 0.7) +
  geom_line(aes(y = maxcumulativereturn, group = 1, color = "Max Cumulative Return"), linetype = "dashed") +
  scale_y_continuous(name = "Cumulative return", limits = c(min(appleData$cumulativereturn)-0.2, max(appleData$cumulativereturn)+0.2), expand = c(0, 0))+
  scale_color_manual(values = c("Cumulative Return" = "darkgreen", "Max Cumulative Return" = "blue", "Daily Return" = "orange")) +
  theme_economist() +
  ggtitle(paste0("Cumulative Return, Max Cumulative Return and Daily Return of ", "AAPL")) +
  xlab("Date") +
  ylab("Return")+
  guides(color=guide_legend(title="Type of Return"))
p1 <- p1 + theme(panel.grid.major = element_line(color = "gray", size = 0.5),
                 panel.grid.minor = element_line(color = "gray", size = 0.25),
                 plot.title = element_text(size = 12))
p2 <- p2 + theme(panel.grid.major = element_line(color = "gray", size = 0.5),
                 panel.grid.minor = element_line(color = "gray", size = 0.25),
                 plot.title = element_text(size = 12))
p3 <- p3 + theme(panel.grid.major = element_line(color = "gray", size = 0.5),
                 panel.grid.minor = element_line(color = "gray", size = 0.25),
                 plot.title = element_text(size = 12))
p4 <- p4 + theme(panel.grid.major = element_line(color = "gray", size = 0.5),
                 panel.grid.minor = element_line(color = "gray", size = 0.25),
                 plot.title = element_text(size = 12))
grid.arrange(p1, p2, p3, p4, ncol = 2)
