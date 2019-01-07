library(GAS)
library(quantmod)
library(xts)


setwd("/Users/Administrator/Desktop/1/ACF313/Group")
BTVaR <- as.matrix(read.csv("BTport_VaR.csv")$V1)
DefVaR <- as.matrix(read.csv("Defport_VaR.csv")$V1)

dateFrom <- as.Date("2017-01-01")
dateTo <- as.Date("2018-10-01")

diff <- as.numeric(dateTo - dateFrom)
time <- seq.Date(from = dateFrom, to = dateTo-1, by = "day")

maxDate <- as.Date("2017-01-02")
endDate <- as.Date("2017-01-04")

alpha <- 0.05

BTresult <- array(NA,diff)
Defresult <- array(NA,diff)

for (i in 1:diff) {
  AAPL_adj <- tryCatch(Ad(getSymbols("AAPL", auto.assign = F, src = "yahoo", from = maxDate, to = endDate)),error =function(e) 0)
  MSFT_adj <- tryCatch(Ad(getSymbols("MSFT", auto.assign = F, src = "yahoo", from = maxDate, to = endDate)),error =function(e) 0)
  Goog_adj <- tryCatch(Ad(getSymbols("GOOG", auto.assign = F, src = "yahoo", from = maxDate, to = endDate)),error =function(e) 0)
  Csco_adj <- tryCatch(Ad(getSymbols("CSCO", auto.assign = F, src = "yahoo", from = maxDate, to = endDate)),error =function(e) 0)
  
  DOC_adj <- tryCatch(Ad(getSymbols("DOC", auto.assign = F, src = "yahoo", from = maxDate, to = endDate)),error =function(e) 0)
  NEE_adj <- tryCatch(Ad(getSymbols("NEE", auto.assign = F, src = "yahoo", from = maxDate, to = endDate)),error =function(e) 0)
  AEP_adj <- tryCatch(Ad(getSymbols("AEP", auto.assign = F, src = "yahoo", from = maxDate, to = endDate)),error =function(e) 0)
  JNJ_adj <- tryCatch(Ad(getSymbols("JNJ", auto.assign = F, src = "yahoo", from = maxDate, to = endDate)),error =function(e) 0)
  
  retAAPL<-as.numeric(diff(log(AAPL_adj))[-1])
  retMSFT<-as.numeric(diff(log(MSFT_adj))[-1])
  retGoog<-as.numeric(diff(log(Goog_adj))[-1])
  retCsco<-as.numeric(diff(log(Csco_adj))[-1])
  
  retDOC<-as.numeric(diff(log(DOC_adj))[-1])
  retNEE<-as.numeric(diff(log(NEE_adj))[-1])
  retAEP<-as.numeric(diff(log(AEP_adj))[-1])
  retJNJ<-as.numeric(diff(log(JNJ_adj))[-1])
  
  BTVaR_test <- as.numeric(retAAPL+retMSFT+retGoog+retCsco)/4
  # BTcount <- length(BTVaR_test[BTVaR_test< BTVaR[i]])
  
  DefVaR_test <- as.numeric(retDOC+retNEE+retAEP+retJNJ)/4
  # Defcount <- length(BTVaR_test[BTVaR_test< BTVaR[i]])
  
  # n<- length(BTVaR_test)
  
  # if(BTcount < n*alpha){
  #   BTresult[i] <- "success"
  # }else{
  #   BTresult[i] <- "fail"
  # }
  # 
  # if(Defcount < n*alpha){
  #   Defresult[i] <- "success"
  # }else{
  #   Defresult[i] <- "fail"
  # }
  
  if(length(BTVaR_test) == 0){
    BTresult[i] <- "No output"
  }else{
    if(BTVaR_test > BTVaR[i]){
    BTresult[i] <- "success"
    }else{
    BTresult[i] <- "fail"
    }
  }
  
  
  if(length(DefVaR_test) == 0){
    Defresult[i] <- "No output"
  }else{
    if(DefVaR_test > DefVaR[i]){
    Defresult[i] <- "success"
    }else{
    Defresult[i] <- "fail"
    }
  }
  
  print(i)
  
  maxDate = maxDate + 1
  endDate = endDate + 1
}

# Defresult <- xts(Defresult,time)
# BTresult <- xts(BTresult,time)
# 
# write.table (Defresult,file ="Devresult.csv", sep =",") 
# write.table (BTresult,file ="BTresult.csv", sep =",") 



# setwd("/Users/Administrator/Desktop/1/ACF313/Group")
# Defresult <- read.csv("Devresult.csv")
# BTresult <- read.csv("BTresult.csv")
# 
# length(Defresult[Defresult=="success"])
# length(Defresult[Defresult=="fail"])
# 
# length(BTresult[BTresult=="success"])
# length(BTresult[BTresult=="fail"])
# 
# as.Date(.indexDate(Defresult[Defresult=="fail"]))
