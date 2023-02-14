#Package Installation

install.packages(c("xts","base","tidyverse","lubridate","zoo","quantmod","ggplot2","ggthemes","gridExtra"))

#HW 1 Answer 1
library(base)
library(tidyverse)
library(lubridate)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
rm(list=ls())

load('OHLC.RData')
sectors<-read_csv(file = 'sectors.csv')

symbolList <- stock[,1]
tradingDays <- as.data.frame(table(symbolList))
maximumTradingDays <- max(tradingDays$Freq)
print(maximumTradingDays)
symbolList <- tradingDays[tradingDays$Freq == maximumTradingDays,1]

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
library(base)
library(tidyverse)
library(lubridate)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())

load('OHLC.RData')
sectors<-read_csv(file = 'sectors.csv')

symbolList <- stock[,1]
tradingDays <- as.data.frame(table(symbolList))
maximumTradingDays <- max(tradingDays$Freq)
symbolList <- tradingDays[tradingDays$Freq == maximumTradingDays,1]
sectorsTemp <- data.frame(sectors)
sectorsTemp$annualreturns = NA

for(symbol in sectorsTemp[,1]){
  if(symbol %in% symbolList){
  open = head(stock[stock$symbol == symbol,3],1)
  close = tail(stock[stock$symbol == symbol,6],1)
  sectorsTemp$annualreturns[sectorsTemp$symbol==symbol]<-round(((close-open)/open)*100,1)
  }
}

ans2 <- aggregate(sectorsTemp$annualreturns, list(sectorsTemp$sector),FUN=mean, na.rm = TRUE)

colnames(ans2)[1] = "Sector"
colnames(ans2)[2] = "AnnualReturn"
ans2$AnnualReturn <- round(ans2$AnnualReturn,1)
ans2<-ans2[order(-ans2$AnnualReturn),]
ans2

#HW 1 Answer 3 
library(base)
library(tidyverse)
library(lubridate)
library(zoo)
library(xts)
library(quantmod)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())

load('OHLC.RData')
sectors<-read_csv(file = 'sectors.csv')

symbolList <- stock[,1]
tradingDays <- as.data.frame(table(symbolList))
maximumTradingDays <- max(tradingDays$Freq)
symbolList <- tradingDays[tradingDays$Freq == maximumTradingDays,1]
stockFiltered <- stock[stock$symbol %in% symbolList, ]

sectorsFiltered <- data.frame(sectors)
sectorsFiltered <- sectorsFiltered[sectorsFiltered$symbol %in% symbolList, ]

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for (i in 1:nrow(sectorsFiltered)) {
  symbol <- sectorsFiltered[i, "symbol"]
  symbolData <- stockFiltered[stockFiltered$symbol== symbol, ]
  symbolXts <- xts(symbolData[,3:7],order.by=as.Date(symbolData$date))
  # Calculate the monthly returns
  symbolMonthlyReturns <- monthlyReturn(symbolXts, type="arithmetic")
  sectorsFiltered[i, paste0(months, "_Return")] <- as.vector(symbolMonthlyReturns)
}

ans3 <- aggregate(sectorsFiltered[, paste0(months, "_Return")], by = list(sectorsFiltered$sector), FUN = mean)

ans3[, 2:13] <- round(ans3[, 2:13] * 100, 4)
colnames(ans3)[2:13] <- paste0(months, "_Return")
ans3$sector <- as.factor(ans3$Group.1)
ans3 <- ans3[, c("sector", paste0(months, "_Return"))]
row.names(ans3) <- ans3[, 1]
ans3Matrix <- as.matrix(ans3[, -1])
colnames(ans3Matrix) <- colnames(ans3[, -1])

ans3Matrix


#HW 1 Answer 4
library(base)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(gridExtra)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(list=ls())

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
  scale_y_continuous(name = "Return", limits = c(min(appleData$cumulativereturn)-0.2, max(appleData$cumulativereturn)+0.2), expand = c(0, 0))+
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
