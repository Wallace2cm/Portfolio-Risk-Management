library("copula")
library("fitdistrplus")
library(quantmod)

dateFrom <- as.Date("2017-01-01")
dateTo <- as.Date("2018-10-01")

diff <- as.numeric(dateTo - dateFrom)
BTport_VaR <-matrix(NA,diff)
Defport_VaR <-matrix(NA,diff)
time <- seq.Date(from = as.Date("2017/01/01",format = "%Y/%m/%d"), by = "day", length.out = diff)

dt_G <- function(x, mean, sd, nu){#density
  dt((x-mean)/sd,nu)/sd
}
pt_G <- function(x, mean, sd, nu){#CDF
  pt((x-mean)/sd,nu)
}
qt_G <- function(x, mean, sd, nu){#quantile
  qt(x,nu)*sd+mean
}

resFunc_BT<- function(x){
  for(k in 1:N){
    z <- rnorm(d)
    z_tilde <- L_BT%*%z
    
    R_AAPL <- exp(qt_G(pnorm(z_tilde[1]), mean=mean_AAPL,
                          sd=sd_AAPL, nu=nu_AAPL))
    
    R_MSFT <- exp(qt_G(pnorm(z_tilde[2]), mean=mean_MSFT,
                          sd=sd_MSFT, nu=nu_MSFT))
    
    R_Goog <- exp(qt_G(pnorm(z_tilde[3]), mean=mean_Goog,
                          sd=sd_Goog, nu=nu_Goog))
    
    R_Csco <- exp(qt_G(pnorm(z_tilde[4]), mean=mean_Csco,
                          sd=sd_Csco, nu=nu_Csco))
    
    res[k] <- 1*(0.25*R_AAPL+0.25*R_Goog+0.25*R_Csco+0.25*R_MSFT<x)
  }
  mean(res)-alpha
}

resFunc_Def<- function(x){
  for(k in 1:N){
    z <- rnorm(d)
    z_tilde <- L_Def%*%z
    
    R_DOC <- exp(qt_G(pnorm(z_tilde[1]), mean=mean_DOC,
                          sd=sd_DOC, nu=nu_DOC))
    
    R_NEE <- exp(qt_G(pnorm(z_tilde[2]), mean=mean_NEE,
                          sd=sd_NEE, nu=nu_NEE))
    
    R_AEP <- exp(qt_G(pnorm(z_tilde[3]), mean=mean_AEP,
                          sd=sd_AEP, nu=nu_AEP))
    
    R_JNJ <- exp(qt_G(pnorm(z_tilde[4]), mean=mean_JNJ,
                          sd=sd_JNJ, nu=nu_JNJ))
    
    res[k] <- 1*(0.25*R_DOC+0.25*R_NEE+0.25*R_AEP+0.25*R_JNJ<x)
  }
  mean(res)-alpha
}


search_x_BT <- function(a=0.7, b=1, err.tol=0.0001){
  fa <- resFunc_BT(a)
  fb <- resFunc_BT(b)
  while(abs(b-a) > err.tol){
    ab <- (a+b)/2
    fab <- resFunc_BT(ab)
    if((fab*fa)<0){
      b <- ab
      fb <- fab
    }else{
      a <- ab
      fa <- fab
    }
  }
  (a+b)/2
}

search_x_Def <- function(a=0.7, b=1, err.tol=0.0001){
  fa <- resFunc_Def(a)
  fb <- resFunc_Def(b)
  while(abs(b-a) > err.tol){
    ab <- (a+b)/2
    fab <- resFunc_Def(ab)
    if((fab*fa)<0){
      b <- ab
      fb <- fab
    }else{
      a <- ab
      fa <- fab
    }
  }
  (a+b)/2
}

d <- 4
alpha <- 0.05
N<-10000
res<-array(NA,dim=N)

norm.cop <- normalCopula(dim=4,dispstr="un")
maxDate <- as.Date("2016-01-01")
endDate <- as.Date("2017-01-01")

for(i in 1: diff){
  AAPL_adj <- Ad(getSymbols("AAPL", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
  MSFT_adj <- Ad(getSymbols("MSFT", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
  Goog_adj <- Ad(getSymbols("GOOG", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
  Csco_adj <- Ad(getSymbols("CSCO", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))

  DOC_adj <- Ad(getSymbols("DOC", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
  NEE_adj <- Ad(getSymbols("NEE", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
  AEP_adj <- Ad(getSymbols("AEP", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
  JNJ_adj <- Ad(getSymbols("JNJ", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
  
  
  retAAPL<-as.numeric(diff(log(AAPL_adj))[-1])
  retMSFT<-as.numeric(diff(log(MSFT_adj))[-1])
  retGoog<-as.numeric(diff(log(Goog_adj))[-1])
  retCsco<-as.numeric(diff(log(Csco_adj))[-1])

  retDOC<-as.numeric(diff(log(DOC_adj))[-1])
  retNEE<-as.numeric(diff(log(NEE_adj))[-1])
  retAEP<-as.numeric(diff(log(AEP_adj))[-1])
  retJNJ<-as.numeric(diff(log(JNJ_adj))[-1])
  
  ft_AAPL <- fitdist(retAAPL,"t_G",
                   start=list(mean=mean(retAAPL),
                              sd=sd(retAAPL),
                              nu=5))
  ft_MSFT <- fitdist(retMSFT,"t_G",
                   start=list(mean=mean(retAAPL),
                              sd=sd(retAAPL),
                              nu=5))
  ft_Goog <- fitdist(retGoog,"t_G",
                   start=list(mean=mean(retAAPL),
                              sd=sd(retAAPL),
                              nu=5))
  ft_Csco <- fitdist(retCsco,"t_G",
                   start=list(mean=mean(retAAPL),
                              sd=sd(retAAPL),
                              nu=5))

  ft_DOC <- fitdist(retDOC,"t_G",
                     start=list(mean=mean(retDOC),
                                sd=sd(retDOC),
                                nu=5))
  ft_NEE <- fitdist(retNEE,"t_G",
                     start=list(mean=mean(retNEE),
                                sd=sd(retNEE),
                                nu=5))
  ft_AEP <- fitdist(retAEP,"t_G",
                     start=list(mean=mean(retAEP),
                                sd=sd(retAEP),
                                nu=5))
  ft_JNJ <- fitdist(retJNJ,"t_G",
                     start=list(mean=mean(retJNJ),
                                sd=sd(retJNJ),
                                nu=5))
  
  

  u_BT<-matrix(nrow=length(retAAPL),ncol=4)
  u_BT[,1]<-pt_G(retAAPL,
               mean=as.list(ft_AAPL$estimate)$mean,
               sd=as.list(ft_AAPL$estimate)$sd,
               nu=as.list(ft_AAPL$estimate)$nu)

  u_BT[,2]<-pt_G(retMSFT,
               mean=as.list(ft_MSFT$estimate)$mean,
               sd=as.list(ft_MSFT$estimate)$sd,
               nu=as.list(ft_MSFT$estimate)$nu)

  u_BT[,3]<-pt_G(retGoog,
               mean=as.list(ft_Goog$estimate)$mean,
               sd=as.list(ft_Goog$estimate)$sd,
               nu=as.list(ft_Goog$estimate)$nu)

  u_BT[,4]<-pt_G(retCsco,
               mean=as.list(ft_Csco$estimate)$mean,
               sd=as.list(ft_Csco$estimate)$sd,
               nu=as.list(ft_Csco$estimate)$nu)

  u_Def<-matrix(nrow=length(retDOC),ncol=4)
  u_Def[,1]<-pt_G(retDOC,
                    mean=as.list(ft_DOC$estimate)$mean,
                    sd=as.list(ft_DOC$estimate)$sd,
                    nu=as.list(ft_DOC$estimate)$nu)
  
  u_Def[,2]<-pt_G(retNEE,
                    mean=as.list(ft_NEE$estimate)$mean,
                    sd=as.list(ft_NEE$estimate)$sd,
                    nu=as.list(ft_NEE$estimate)$nu)
  
  u_Def[,3]<-pt_G(retAEP,
                    mean=as.list(ft_AEP$estimate)$mean,
                    sd=as.list(ft_AEP$estimate)$sd,
                    nu=as.list(ft_AEP$estimate)$nu)
  
  u_Def[,4]<-pt_G(retJNJ,
                    mean=as.list(ft_JNJ$estimate)$mean,
                    sd=as.list(ft_JNJ$estimate)$sd,
                    nu=as.list(ft_JNJ$estimate)$nu)


  n.cop_BT <- fitCopula(norm.cop,u_BT,method="ml")
  
  n.cop_Def <- fitCopula(norm.cop,u_Def,method="ml")

  matrixdata_BT<-as.numeric(coef(n.cop_BT))
  matrixdata_Def<-as.numeric(coef(n.cop_Def))

  mean_AAPL <- as.list(ft_AAPL$estimate)$mean
  sd_AAPL <- as.list(ft_AAPL$estimate)$sd
  nu_AAPL <- as.list(ft_AAPL$estimate)$nu

  mean_MSFT <- as.list(ft_MSFT$estimate)$mean
  sd_MSFT <- as.list(ft_MSFT$estimate)$sd
  nu_MSFT <- as.list(ft_MSFT$estimate)$nu

  mean_Goog <- as.list(ft_Goog$estimate)$mean
  sd_Goog <- as.list(ft_Goog$estimate)$sd
  nu_Goog <- as.list(ft_Goog$estimate)$nu

  mean_Csco <- as.list(ft_Csco$estimate)$mean
  sd_Csco <- as.list(ft_Csco$estimate)$sd
  nu_Csco <- as.list(ft_Csco$estimate)$nu
 
  
  mean_DOC <- as.list(ft_DOC$estimate)$mean
  sd_DOC <- as.list(ft_DOC$estimate)$sd
  nu_DOC <- as.list(ft_DOC$estimate)$nu
  
  mean_NEE <- as.list(ft_NEE$estimate)$mean
  sd_NEE <- as.list(ft_NEE$estimate)$sd
  nu_NEE <- as.list(ft_NEE$estimate)$nu
  
  mean_AEP <- as.list(ft_AEP$estimate)$mean
  sd_AEP <- as.list(ft_AEP$estimate)$sd
  nu_AEP <- as.list(ft_AEP$estimate)$nu
  
  mean_JNJ <- as.list(ft_JNJ$estimate)$mean
  sd_JNJ <- as.list(ft_JNJ$estimate)$sd
  nu_JNJ <- as.list(ft_JNJ$estimate)$nu
  
  cor_BT<-matrix(data=c(1,matrixdata_BT[1],matrixdata_BT[2],matrixdata_BT[3],
                        matrixdata_BT[1],1,matrixdata_BT[4],matrixdata_BT[5],
                        matrixdata_BT[3],matrixdata_BT[4],1,matrixdata_BT[6],
                        matrixdata_BT[3],matrixdata_BT[5],matrixdata_BT[6],1),nrow=4,ncol=4)

  cor_Def<-matrix(data=c(1,matrixdata_Def[1],matrixdata_Def[2],matrixdata_Def[3],
                        matrixdata_Def[1],1,matrixdata_Def[4],matrixdata_Def[5],
                        matrixdata_Def[3],matrixdata_Def[4],1,matrixdata_Def[6],
                        matrixdata_Def[3],matrixdata_Def[5],matrixdata_Def[6],1),nrow=4,ncol=4)
  
  
  L_BT <- t(chol(cor_BT))
  L_Def <- t(chol(cor_Def))
  
  seed <- sample(1000:5000,1)
  set.seed(seed)

  BTport_VaR[i] <- search_x_BT()-1
  Defport_VaR[i] <- search_x_Def()-1

  maxDate = maxDate + 1
  endDate = endDate + 1

  print(paste(i,"95%:","Big Tech:", BTport_VaR[i],
                       "Defensive:", Defport_VaR[i]))

}

time <- seq.Date(from = as.Date("2017/01/01",format = "%Y/%m/%d"), by = "day", length.out = diff)

BTVaR <-xts(BTport_VaR,time)
DefVaR <-xts(Defport_VaR,time)

Sys.setlocale("LC_TIME", "English") 

plot(BTVaR,main = "Big Tech VaR")
plot(DefVaR,main = "Defensive VaR")

plot(BTVaR,main = "Big Tech VaR versus Defensive VaR",ylim = c(-0.020,-0.005))
lines(DefVaR,lwd = 2,col = "red")

write.table (BTport_VaR,file ="BTport_VaR.csv", sep =",") 
write.table (Defport_VaR,file ="Defport_VaR.csv", sep =",") 
