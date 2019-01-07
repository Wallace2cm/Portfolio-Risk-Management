library(quantmod)
library(xts)
library(copula)
library(fitdistrplus)

setwd("/Users/Administrator/Desktop/1/ACF313/Group")
BTVaR <- read.csv("BTport_VaR.csv")
DefVaR <- read.csv("Defport_VaR.csv")

maxDate <- as.Date("2017-01-01")
endDate <- as.Date("2018-10-01")

time <- seq.Date(from = maxDate, to = endDate-1, by = "day")


AAPL_adj <- Ad(getSymbols("AAPL", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
MSFT_adj <- Ad(getSymbols("MSFT", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
Goog_adj <- Ad(getSymbols("GOOG", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
Csco_adj <- Ad(getSymbols("CSCO", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
  
DOC_adj <- Ad(getSymbols("DOC", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
NEE_adj <- Ad(getSymbols("NEE", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
AEP_adj <- Ad(getSymbols("AEP", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
JNJ_adj <- Ad(getSymbols("JNJ", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
  
  
retAAPL<-diff(log(AAPL_adj))[-1]
retMSFT<-diff(log(MSFT_adj))[-1]
retGoog<-diff(log(Goog_adj))[-1]
retCsco<-diff(log(Csco_adj))[-1]
  
retDOC<-diff(log(DOC_adj))[-1]
retNEE<-diff(log(NEE_adj))[-1]
retAEP<-diff(log(AEP_adj))[-1]
retJNJ<-diff(log(JNJ_adj))[-1]
  
BTportRet<-(retAAPL+retMSFT+retGoog+retCsco)/4
DefportRet<-(retDOC+retNEE+retAEP+retJNJ)/4

BTVaR <-xts(BTVaR,time)
DefVaR <-xts(DefVaR,time)

# BTVaR <- -BTVaR
# BTportRet <- -BTportRet
# 
# DefVaR <- -DefVaR
# DefportRet<- -DefportRet
# 
# Sys.setlocale("LC_TIME", "English") 
# 
# plot(BTVaR,main = "Big Tech VaR versus Real Loss",ylim = c(-0.04,0.04),col="#FF4500",lwd = 1)
# lines(BTportRet,col = "black",lwd = 1)
# 
# plot(DefVaR,main = "Defensive VaR versus Real Loss",ylim = c(-0.03,0.03),col="#FF4500",lwd = 3)
# lines(DefportRet,col = "black",lwd = 2)