library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(xts)
library(rugarch)
library(rmgarch)
library(car)
library(fitdistrplus)
library(cowplot)
library(ggplot2)
library(ellipse)
library(corrplot)
library(scales)
library(expss)
library(copula)
maxDate <- as.POSIXct("2017-01-01", format = "%Y-%m-%d")
endDate <- as.POSIXct("2018-10-01", format = "%Y-%m-%d")
companies <- c("AAPL","MSFT","GOOGL","CSCO")
weight <- c(0.25,0.25,0.25,0.25)

#Get price and calculate the return
Appl_adj <- Ad(getSymbols("AAPL", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
time_apple <- as.data.frame(Appl_adj)
time_apple <- cbind(date=as.Date(rownames(time_apple)),time_apple)
Appl_ret <- dailyReturn(Appl_adj)[-1]

MSFT_adj <- Ad(getSymbols("MSFT", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
time_msft <- as.data.frame(MSFT_adj)
time_msft <- cbind(date=as.Date(rownames(time_msft)),time_msft)
MSFT_ret <- dailyReturn(MSFT_adj)[-1]

Goog_adj <- Ad(getSymbols("GOOG", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
time_goog <- as.data.frame(Goog_adj)
time_goog <- cbind(date=as.Date(rownames(time_goog)),time_goog)
Goog_ret <- dailyReturn(Goog_adj)[-1]

Csco_adj <- Ad(getSymbols("CSCO", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
time_csco <- as.data.frame(Csco_adj)
time_csco <- cbind(date=as.Date(rownames(time_csco)),time_csco)
Csco_ret <- dailyReturn(Csco_adj)[-1]

#Correlation
cor_Appl_MSFT <- cor(Appl_ret, MSFT_ret)
cor_Appl_Goog <- cor(Appl_ret, Goog_ret)
cor_Appl_Csco <- cor(Appl_ret, Csco_ret)
cor_MSFT_Goog <- cor(MSFT_ret, Goog_ret)
cor_MSFT_Csco <- cor(MSFT_ret, Csco_ret)
cor_Goog_Csco <- cor(Goog_ret, Csco_ret)
corelation <- c(cor_Appl_MSFT, cor_Appl_Goog, cor_Appl_Csco, cor_MSFT_Goog, cor_MSFT_Csco, cor_Goog_Csco)

num_company <- length(companies)
cor_ret <- matrix(NA, num_company, num_company)
diag(cor_ret) <- 1

k <- 1
for (i in 1 : (num_company - 1)) {
  for (j in (i+1) : num_company) {
    cor_ret[i,j] <- cor_ret[j,i] <- corelation[k]
    k = k + 1
  }
  k = 1
}
#Plot correlation 


corrplot(cor_ret, method = "square")
#Corvariance
cov_Appl_MSFT <- cov(Appl_ret, MSFT_ret)
cov_Appl_Goog <- cov(Appl_ret, Goog_ret)
cov_Appl_Csco <- cov(Appl_ret, Csco_ret)
cov_MSFT_Goog <- cov(MSFT_ret, Goog_ret)
cov_MSFT_Csco <- cov(MSFT_ret, Csco_ret)
cov_Goog_Csco <- cov(Goog_ret, Csco_ret)
corvariance <- c(cov_Appl_MSFT, cov_Appl_Goog, cov_Appl_Csco, 
                 cov_MSFT_Goog, cov_MSFT_Csco, cov_Goog_Csco)

cov_ret <- matrix(NA, num_company,num_company)
diag(cov_ret) <- c( cov(Appl_ret, Appl_ret), cov(MSFT_ret, MSFT_ret),cov(Goog_ret, Goog_ret), cov(Csco_ret, Csco_ret))

k <- 1
for (i in 1 : (num_company - 1)) {
  for (j in (i+1) : num_company) {
    options(scipen = 999)
    cov_ret[i,j] <- cov_ret[j,i] <- corvariance[k]
    k = k + 1
  }
  k = 1
}

#Plot
Apple <- ggplot() +
  geom_line(aes(x = time_apple$date, y =time_apple$AAPL.Adjusted),
            colour = 'black') +
  ggtitle('APPLE Stock Price') +
  xlab('Price') +
  ylab('Time')

MSFT <- ggplot() +
  geom_line(aes(x = time_msft$date, y =time_msft$MSFT.Adjusted),
            colour = 'black') +
  ggtitle('MSFT Stock Price') +
  xlab('Price') +
  ylab('Time')

Google <- ggplot() +
  geom_line(aes(x = time_goog$date, y =time_goog$GOOG.Adjusted),
            colour = 'black') +
  ggtitle('GOOGLE Stock Price') +
  xlab('Price') +
  ylab('Time')

CSCO <- ggplot() +
  geom_line(aes(x = time_csco$date, y =time_csco$CSCO.Adjusted),
            colour = 'black') +
  ggtitle('CSCO Stock Price') +
  xlab('Price') +
  ylab('Time')

plot_grid(Apple, MSFT, Google, CSCO, labels = "AUTO")


#Compute the Beta coefficients
SP500_adj <- Ad(getSymbols("^GSPC", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
time_sp <- as.data.frame(SP500_adj)
time_sp <- cbind(date=as.Date(rownames(time_sp)),time_sp)
SP500_ret <- dailyReturn(SP500_adj)[-1]





beta_Appl <- cov(Appl_ret, SP500_ret)/var(SP500_ret)
beta_MSFT <- cov(MSFT_ret, SP500_ret)/var(SP500_ret)
beta_Goog <- cov(Goog_ret, SP500_ret)/var(SP500_ret)
beta_Csco <- cov(Csco_ret, SP500_ret)/var(SP500_ret)
beta_Port <- sum(c(beta_Appl,beta_MSFT,beta_Goog,beta_Csco)*weight)


rects <- data.frame(xmin=as.Date("2018-01-20"), xmax=as.Date("2018-02-28"), ymin=-Inf, ymax=Inf)


ggplot() +
  geom_line(aes(x = time_sp$date, 
                y = SP500_adj),
            colour = 'black') +
  geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="orange",
            fill = "orange",
            alpha=0.5,
            inherit.aes = FALSE)+
  ggtitle('SP500') +
  xlab('Price') +
  ylab('Time')









Port_beta <- cbind(beta_Appl, beta_MSFT, beta_Goog, beta_Csco)
colnames(Port_beta) <- companies
rownames(Port_beta) <- "Beta"
#Calculate Alpha
Tbill <- getSymbols("^TNX", src = "yahoo", start = maxDate, to = endDate)
Tbill <- TNX['2017-01-01/2018-10-01']
time_tbill <- as.data.frame(Tbill)
time_tbill <- cbind(date=as.Date(rownames(time_tbill)),time_tbill)

ggplot() +
  geom_line(aes(x = time_tbill$date, 
                y = time_tbill$TNX.Adjusted),
            colour = 'black') +
  ggtitle('10 T Bill') +
  xlab('Price') +
  ylab('Time')

plot(Tbill)


Tbill_ret <- dailyReturn(na.omit(Tbill$TNX.Adjusted))[-1]
RF <- mean(na.omit(Tbill))
Appl_rf <- CAPM.alpha(Appl_ret, SP500_ret, RF)
Goog_rf <- CAPM.alpha(Goog_ret, SP500_ret, RF)
MSFT_rf <- CAPM.alpha(MSFT_ret, SP500_ret, RF)
Csco_rf <- CAPM.alpha(Csco_ret, SP500_ret, RF)
Port_rf <- cbind(Appl_rf, MSFT_rf, Goog_rf, Csco_rf)
colnames(Port_rf) <- companies
rownames(Port_rf) <- "Alpha"


#Calculate VaR by variacne-corvariance method
Port_ret <- cbind(Appl_ret,MSFT_ret, Goog_ret, Csco_ret)
mean_Port <- mean(Port_ret * weight)
sd_Port = sd(Port_ret * weight)
prob = qnorm(0.01)
VaR_cc = mean_Port + prob * sd_Port
var.portfolio(Port_ret, weights = weight)

bigT_ret <- (Port_ret*weight)[,1] + (Port_ret*weight)[,2] +(Port_ret*weight)[,3] + (Port_ret*weight)[,4]


cor(Tbill_ret,bigT_ret)

ggplot() +
  geom_point(aes(x = bigT_ret, 
                 y =Tbill_ret),
             colour = 'black') +
  ggtitle('Correlation between return of 10 Year T Bill and Big TechPortfolio') +
  xlab('Portfolio') +
  ylab('Tbill')



port_price <- cbind(Appl_adj, MSFT_adj, Goog_adj, Csco_adj) * weight


#Treynor Ratio and Sharpe Ratio
trey <- TreynorRatio(bigT_ret, SP500_ret, Rf = RF, scale = 252)

sharpe <- SharpeRatio.annualized(bigT_ret, Rf = RF, 
                                 scale = 252, geometric = TRUE)


#Fit into normal and t distribution
#Define PDF, CDF and quantile of t distribution
dt_G <- function(x, mean, sd, nu){
  dt((x-mean)/sd,nu)/sd
}

pt_G <- function(x, mean, sd, nu){
  pt((x-mean)/sd,nu)
}
# 
qt_G <- function(x, mean, sd, nu){
  qt(x,nu) * sd + mean
}

#Log return
Appl_log <- as.numeric(na.omit(diff(log(Appl_adj))))
Appl_G <- fitdist(Appl_log, "norm", start = list(mean = mean(Appl_log), sd = sd(Appl_log)))
summary(Appl_G)[6:7]
Appl_t <- fitdist(Appl_log, "t_G", start = list(mean = mean(Appl_log), sd = sd(Appl_log), nu = 5))
summary(Appl_t)[6:7]

MSFT_log <- as.numeric(na.omit(diff(log(MSFT_adj))))
MSFT_G <- fitdist(MSFT_log, "norm", start = list(mean = mean(MSFT_log), sd = sd(MSFT_log)))
summary(MSFT_G)[6:7]
MSFT_t <- fitdist(MSFT_log, "t_G", start = list(mean = mean(MSFT_log), sd = sd(MSFT_log), nu = 5))
summary(MSFT_t)[6:7]

Goog_log <- as.numeric(na.omit(diff(log(Goog_adj))))
Goog_G <- fitdist(Goog_log, "norm", start = list(mean = mean(Goog_log), sd = sd(Goog_log)))
summary(Goog_G)[6:7]
Goog_t <- fitdist(Goog_log, "t_G", start = list(mean = mean(Goog_log), sd = sd(Goog_log), nu = 5))
summary(Goog_t)[6:7]

Csco_log <- as.numeric(na.omit(diff(log(Csco_adj))))
Csco_G <- fitdist(Csco_log, "norm", start = list(mean = mean(Csco_log), sd = sd(Csco_log)))
summary(Csco_G)[6:7]
Csco_t <- fitdist(Csco_log, "t_G", start = list(mean = mean(Csco_log), sd = sd(Csco_log), nu = 5))
summary(Csco_t)[6:7]





table_G <- rbind(summary(Appl_G)[6:7],summary(MSFT_G)[6:7],summary(Goog_G)[6:7],summary(Csco_G)[6:7])
table_t <- rbind(summary(Appl_t)[6:7],summary(MSFT_t)[6:7],summary(Goog_t)[6:7],summary(Csco_t)[6:7])

table_compare <- cbind(table_G, table_t)
rownames(table_compare) <- companies



#histogram plot and density of t and Gaussian
histt <- function(data, ft, location = "topleft", legend.cex = 1, xlab){
  # parameters of the fitted t distribution
  mean_t <- as.list(ft$estimate)$mean
  sd_t <- as.list(ft$estimate)$sd
  nu_t <- as.list(ft$estimate)$nu
  
  # drawing histogram and assigning to h so that we can get the breakpoints between histogram cells (we will use it!)
  h <- hist(data, breaks=30)
  
  # x sequence for the additional plots on the histogram
  x_seq <- seq(-3,3,length=10000)
  
  # y sequence: density of fitted t distr. at x_seq
  yhistt <- dt_G(x_seq, mean=mean_t, sd=sd_t, nu=nu_t)
  
  # y sequence: density of normal distr. with mean and standard deviation of log-returns at x_seq
  # Note. I did not fit the normal distribution as we would get almost the same mean and standard deviation
  yhistNorm <- dnorm(x_seq, mean=mean(data), sd=sd((data)))
  
  # drawing histogram but this time we draw its density as y axis (freq=FALSE) 
  hist(data,freq=FALSE,xlab=xlab, ylab="Density", breaks=h$breaks, main=paste(""),cex.lab=1.5)
  
  # adding the density of the fitted t distribution at x_seq
  lines(x_seq, yhistt, col=4)
  # adding the density of the normal distribution at x_seq	
  lines(x_seq, yhistNorm, lty = "dashed")
  
  # Legend 
  tmp.text <- c("t", "Gaussian")
  legend(location, legend = tmp.text, cex = legend.cex, lty = c(1,2), col=c(4,1))	
}

histt(Appl_log, Appl_t, xlab="Daily log returns of Appl")
histt(MSFT_log, MSFT_t, xlab="Daily log returns of MSFT")
histt(Goog_log, Goog_t, xlab="Daily log returns of Goog")
histt(Csco_log, Csco_t, xlab="Daily log returns of Csco")

#Create the matrix of correlation of normal and t, compute the copula
company_matrix <- matrix(nrow = length(Appl_log) , ncol = 4)
company_matrix[,1] <- pt_G(Appl_log,
                           mean = as.list(Appl_t$estimate)$mean,
                           sd = as.list(Appl_t$estimate)$sd,
                           nu = as.list(Appl_t$estimate)$nu)
company_matrix[,2] <- pt_G(MSFT_log,
                           mean = as.list(MSFT_t$estimate)$mean,
                           sd = as.list(MSFT_t$estimate)$sd,
                           nu = as.list(MSFT_t$estimate)$nu)
company_matrix[,3] <- pt_G(Goog_log,
                           mean = as.list(Goog_t$estimate)$mean,
                           sd = as.list(Goog_t$estimate)$sd,
                           nu = as.list(Goog_t$estimate)$nu)
company_matrix[,4] <- pt_G(Csco_log,
                           mean = as.list(Csco_t$estimate)$mean,
                           sd = as.list(Csco_t$estimate)$sd,
                           nu = as.list(Csco_t$estimate)$nu)

norm.cop <- normalCopula(dim = 4,dispstr="un")
n.cop <- fitCopula(norm.cop,company_matrix,method="ml")
n.cop

t.cop <- tCopula(dim = 4, dispstr = "un")
t.cop <- fitCopula(t.cop, company_matrix, method = "ml")
t.cop

num_company <- length(companies)
cor_norm <- matrix(NA, num_company, num_company)
diag(cor_norm) <- 1
k <- 1
for (i in 1 : (num_company - 1)) {
  for (j in (i+1) : num_company) {
    cor_norm[i,j] <- cor_norm[j,i] <- as.numeric(coef(n.cop)[k])
    k = k + 1
  }
  k = 1
}


cor_t <- matrix(NA, num_company, num_company)
diag(cor_t) <- 1
for (i in 1 : (num_company - 1)) {
  for (j in (i+1) : num_company) {
    cor_t[i,j] <- cor_t[j,i] <- as.numeric(coef(t.cop)[k])
    k = k + 1
  }
  k = 1
}


#Defensive Portfolio
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
companies_defensive <- c("PRT","NEE","AEP","JNJ")
weight_defensive <- c(0.25,0.25,0.25,0.25)
#Get price and calculate the return
#Defensive portfolio
Prt_adj <- Ad(getSymbols("DOC", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
time_Prt <- as.data.frame(Prt_adj)
time_Prt <- cbind(date=as.Date(rownames(time_Prt)),time_Prt)
Prt_ret <- dailyReturn(Prt_adj)[-1]

Nee_adj <- Ad(getSymbols("NEE", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
time_Nee <- as.data.frame(Nee_adj)
time_Nee <- cbind(date=as.Date(rownames(time_Nee)),time_Nee)
Nee_ret <- dailyReturn(Nee_adj)[-1]


Aep_adj <- Ad(getSymbols("AEP", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
time_Aep <- as.data.frame(Aep_adj)
time_Aep <- cbind(date=as.Date(rownames(time_Aep)),time_Aep)
Aep_ret <- dailyReturn(Aep_adj)[-1]

JNJ_adj <- Ad(getSymbols("JNJ", auto.assign = F, src = "yahoo", from = maxDate, to = endDate))
time_JNJ <- as.data.frame(JNJ_adj)
time_JNJ <- cbind(date=as.Date(rownames(time_JNJ)),time_JNJ)
JNJ_ret <- dailyReturn(JNJ_adj)[-1]

#Compute Correlation of defensive stocks
cor_Prt_Nee <- cor(Prt_ret, Nee_ret)
cor_Prt_Aep <- cor(Prt_ret, Aep_ret)
cor_Prt_JNJ <- cor(Prt_ret, JNJ_ret)
cor_Nee_Aep <- cor(Nee_ret, Aep_ret)
cor_Nee_JNJ <- cor(Nee_ret, JNJ_ret)
cor_Aep_JNJ <- cor(Aep_ret, JNJ_ret)
corelation_def <- c(cor_Prt_Nee,cor_Prt_Aep,cor_Prt_JNJ,cor_Nee_Aep,cor_Nee_JNJ,cor_Aep_JNJ)

num_company_def <- length(companies_defensive)
cor_ret_def <- matrix(NA, num_company_def, num_company_def)
diag(cor_ret_def) <- 1

k <- 1
for (i in 1 : (num_company_def - 1)) {
  for (j in (i+1) : num_company_def) {
    cor_ret_def[i,j] <- cor_ret_def[j,i] <- corelation_def[k]
    k = k + 1
  }
  k = 1
}
#Plot correlation 


# corrplot(cor_ret_def, method = "square")
#Corvariance
cov_Prt_Nee <- cov(Prt_ret, Nee_ret)
cov_Prt_Aep <- cov(Prt_ret, Aep_ret)
cov_Prt_JNJ <- cov(Prt_ret, JNJ_ret)
cov_Nee_Aep <- cov(Nee_ret, Aep_ret)
cov_Nee_JNJ <- cov(Nee_ret, JNJ_ret)
cov_Aep_JNJ <- cov(Aep_ret, JNJ_ret)
corvariacne_def <- c(cov_Prt_Nee,cov_Prt_Aep,cov_Prt_JNJ,
                     cov_Nee_Aep,cov_Nee_JNJ,cov_Aep_JNJ)

cov_ret_def <- matrix(NA, num_company_def,num_company_def)
diag(cov_ret_def) <- c( cov(Prt_ret, Prt_ret), cov(Nee_ret, Nee_ret),cov(Aep_ret, Aep_ret), cov(JNJ_ret, JNJ_ret))

k <- 1
for (i in 1 : (num_company_def - 1)) {
  for (j in (i+1) : num_company_def) {
    options(scipen = 999)
    cov_ret_def[i,j] <- cov_ret_def[j,i] <- corvariacne_def[k]
    k = k + 1
  }
  k = 1
}

#Plot
PRT <- ggplot() +
  geom_line(aes(x = time_Prt$date, y =time_Prt$DOC.Adjusted),
            colour = 'black') +
  ggtitle('PRT Stock Price') +
  xlab('Price') +
  ylab('Time')

NEE <- ggplot() +
  geom_line(aes(x = time_Nee$date, y =time_Nee$NEE.Adjusted),
            colour = 'black') +
  ggtitle('NEE Stock Price') +
  xlab('Price') +
  ylab('Time')

AEP <- ggplot() +
  geom_line(aes(x = time_Aep$date, y =time_Aep$AEP.Adjusted),
            colour = 'black') +
  ggtitle('AEP Stock Price') +
  xlab('Price') +
  ylab('Time')

JNJ <- ggplot() +
  geom_line(aes(x = time_JNJ$date, y =time_JNJ$JNJ.Adjusted),
            colour = 'black') +
  ggtitle('JNJ Stock Price') +
  xlab('Price') +
  ylab('Time')

plot_grid(PRT, NEE, AEP, JNJ, labels = "AUTO")

#Beta of defensive portfolio
beta_Prt <- cov(Prt_ret, SP500_ret)/var(SP500_ret)
beta_Nee <- cov(Nee_ret, SP500_ret)/var(SP500_ret)
beta_Aep <- cov(Aep_ret, SP500_ret)/var(SP500_ret)
beta_JNJ <- cov(JNJ_ret, SP500_ret)/var(SP500_ret)
beta_Port_def <- sum(c(beta_Prt,beta_Nee,beta_Aep,beta_JNJ)*weight_defensive)

Port_beta_def <- cbind(beta_Prt, beta_Nee, beta_Aep, beta_JNJ)
colnames(Port_beta_def) <- companies_defensive
rownames(Port_beta_def) <- "Beta"


#Alpha of defensive portfolio
Prt_rf <- CAPM.alpha(Prt_ret, SP500_ret, RF)
Nee_rf <- CAPM.alpha(Nee_ret, SP500_ret, RF)
Aep_rf <- CAPM.alpha(Aep_ret, SP500_ret, RF)
JNJ_rf <- CAPM.alpha(JNJ_ret, SP500_ret, RF)
Port_rf_def <- cbind(Prt_rf, Nee_rf, Aep_rf, JNJ_rf)
colnames(Port_rf_def) <- companies_defensive
rownames(Port_rf_def) <- "Alpha"

#Calculate defensive portfolio variance
Port_ret_def <- cbind(Prt_ret, Nee_ret, Aep_ret, JNJ_ret)
mean_Port_def <- mean(Port_ret_def * weight_defensive)
sd_Port_def = sd(Port_ret_def * weight)
prob = qnorm(0.01)

var.portfolio(Port_ret_def, weights = weight_defensive)

def_ret <- (Port_ret_def*weight_defensive)[,1] + (Port_ret_def*weight_defensive)[,2] + (Port_ret_def*weight_defensive)[,3] + (Port_ret_def*weight_defensive)[,4]

cor(def_ret, Tbill_ret)


ggplot() +
  geom_point(aes(x = def_ret, 
                 y =Tbill_ret),
             colour = 'black') +
  ggtitle('Correlation between return of 10 YearT Bill and Defensive Portfolio') +
  xlab('Defensive Portfolio') +
  ylab('Tbill')



port_price_def <- cbind(Prt_adj, Nee_adj, Aep_adj, JNJ_adj) * weight_defensive

rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100

# Log return of Defensive portfolio
Prt_log <- as.numeric(na.omit(diff(log(Prt_adj))))
Prt_G <- fitdist(Prt_log, "norm", start = list(mean = mean(Prt_log), sd = sd(Prt_log)))
summary(Prt_G)[6:7]
Prt_t <- fitdist(Prt_log, "t_G", start = list(mean = mean(Prt_log), sd = sd(Prt_log), nu = 5))
summary(Appl_t)[6:7]

Nee_log <- as.numeric(na.omit(diff(log(Nee_adj))))
Nee_G <- fitdist(Nee_log, "norm", start = list(mean = mean(Nee_log), sd = sd(Nee_log)))
summary(Nee_G)[6:7]
Nee_t <- fitdist(Nee_log, "t_G", start = list(mean = mean(Nee_log), sd = sd(Nee_log), nu = 5))
summary(Nee_t)[6:7]

Aep_log <- as.numeric(na.omit(diff(log(Aep_adj))))
Aep_G <- fitdist(Aep_log, "norm", start = list(mean = mean(Aep_log), sd = sd(Aep_log)))
summary(Aep_G)[6:7]
Aep_t <- fitdist(Aep_log, "t_G", start = list(mean = mean(Aep_log), sd = sd(Aep_log), nu = 5))
summary(Aep_t)[6:7]

JNJ_log <- as.numeric(na.omit(diff(log(JNJ_adj))))
JNJ_G <- fitdist(JNJ_log, "norm", start = list(mean = mean(JNJ_log), sd = sd(JNJ_log)))
summary(JNJ_G)[6:7]
JNJ_t <- fitdist(JNJ_log, "t_G", start = list(mean = mean(JNJ_log), sd = sd(JNJ_log), nu = 5))
summary(JNJ_t)[6:7]

#plot normal, t distribution
histt(Prt_log, Prt_t, xlab="Daily log returns of PRT")
histt(Nee_log, Nee_t, xlab="Daily log returns of Nee")
histt(Aep_log, Aep_t, xlab="Daily log returns of AEP")
histt(JNJ_log, JNJ_t, xlab="Daily log returns of JNJ")


company_matrix_def <- matrix(nrow = length(Prt_log) , ncol = 4)
company_matrix_def[,1] <- pt_G(Prt_log,
                           mean = as.list(Prt_t$estimate)$mean,
                           sd = as.list(Prt_t$estimate)$sd,
                           nu = as.list(Prt_t$estimate)$nu)
company_matrix_def[,2] <- pt_G(Nee_log,
                           mean = as.list(Nee_t$estimate)$mean,
                           sd = as.list(Nee_t$estimate)$sd,
                           nu = as.list(Nee_t$estimate)$nu)
company_matrix_def[,3] <- pt_G(Aep_log,
                           mean = as.list(Aep_t$estimate)$mean,
                           sd = as.list(Aep_t$estimate)$sd,
                           nu = as.list(Aep_t$estimate)$nu)
company_matrix_def[,4] <- pt_G(JNJ_log,
                           mean = as.list(JNJ_t$estimate)$mean,
                           sd = as.list(JNJ_t$estimate)$sd,
                           nu = as.list(JNJ_t$estimate)$nu)

norm.cop.def <- normalCopula(dim = 4,dispstr="un")
n.cop.def <- fitCopula(norm.cop.def,company_matrix_def,method="ml")
n.cop.def

t.cop.def <- tCopula(dim = 4, dispstr = "un")
t.cop.def <- fitCopula(t.cop.def, company_matrix_def, method = "ml")
t.cop.def

num_company_def <- length(companies_defensive)
cor_norm_def <- matrix(NA, num_company_def, num_company_def)
diag(cor_norm_def) <- 1
k <- 1
for (i in 1 : (num_company_def - 1)) {
  for (j in (i+1) : num_company_def) {
    cor_norm_def[i,j] <- cor_norm_def[j,i] <- as.numeric(coef(n.cop.def)[k])
    k = k + 1
  }
  k = 1
}


cor_t_def <- matrix(NA, num_company_def, num_company_def)
diag(cor_t_def) <- 1
for (i in 1 : (num_company_def - 1)) {
  for (j in (i+1) : num_company_def) {
    cor_t_def[i,j] <- cor_t_def[j,i] <- as.numeric(coef(t.cop.def)[k])
    k = k + 1
  }
  k = 1
}
















#Garch
date_big = as.Date(rownames(time_apple))[-1]
blog <- cbind(Appl_log, MSFT_log, Goog_log, Csco_log) * weight
bigT_log <-xts(blog[,1] + blog[,2] + blog[,3] + blog[,4], date_big)




date_def = as.Date(rownames(time_Prt))[-1]
dlog <- cbind(Prt_log, Nee_log, Aep_log, JNJ_log) * weight_defensive
def_log <- xts(dlog[,1] + dlog[,2] + dlog[,3] +dlog[,4], date_def)

sp_log <- xts(SP500_ret, date_big)

gspec.ru <- ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                       variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                       distribution.model="std")
bigTgar <- ugarchfit(gspec.ru, bigT_log)
defgar <- ugarchfit(gspec.ru, def_log)
spgar <- ugarchfit(gspec.ru, sp_log)





coef(bigTgar)
coef(defgar)
# plot volatility estimates


ggplot() +
  geom_line(aes(x= date_big,
                y= sqrt(252) * bigTgar@fit$sigma),
            colour = "#FF3399",
            size = 1) + 
  geom_point(aes(x= date_big,
                 y= sqrt(252) * bigTgar@fit$sigma,colour = "colbig",shape = "colbig"),
             
             size = 2
             ) + 
  geom_line(aes(x= date_def,
                y= sqrt(252) * defgar@fit$sigma),
            colour = "#FF9999",
            size = 1) + 
  geom_point(aes(x= date_def,
                 y= sqrt(252) * defgar@fit$sigma,colour = "coldef",shape = "coldef"),
             size = 2
             )+
  geom_line(aes(x= date_big,
                y= sqrt(252) * spgar@fit$sigma),
            colour = "#0000CC",
            size = 1) + 
  geom_point(aes(x= date_big,
                 y= sqrt(252) * spgar@fit$sigma,colour = "colsp",shape = "colsp"),
             size = 2)+
  scale_x_date(breaks = pretty_breaks(15))+
  scale_colour_manual(name="Volatility", values=c("colbig" = "#FF3399", "coldef"="#FF9999", "colsp" = "#0000CC"), 
                      labels=c("colbig"="Big Tech", "coldef"="Defensive", "colsp" = "SP500"))+
  scale_shape_manual(name="Volatility", values=c("colbig" = 17, "coldef"=19, "colsp" = 18), 
                     labels=c("colbig"="Big Tech", "coldef"="Defensive","colsp" = "SP500")
                     )+
  ggtitle('Garch(1,1), Volatility of Big Tech, Defensive Portfolio and SP500') +
  xlab('Time') +
  ylab('Volatility')










#Treynor Ratio and Sharpe Ratio


trey_def <- TreynorRatio(def_ret,
                         SP500_ret, Rf = RF, scale = 252)

sharpe_def <- SharpeRatio.annualized(def_ret,
                                     Rf = RF, 
                                     scale = 252, geometric = TRUE)


#Treynor Ratio and Sharpe Ratio Comparison

tsR <- cbind(trey, sharpe)
tsR_def <- cbind(trey_def, sharpe_def)
tsR_comp <- rbind(tsR, tsR_def)
colnames(tsR_comp) <- c("Treynor Ratio","Sharpe Ratio")
rownames(tsR_comp) <- c("Big Tech Portfolio", "Defensive Portfolio")

#Compare Drawdowns

com_dd <- merge(bigT_ret,def_ret, SP500_ret)
colnames(com_dd) <- c("Big Tech", "Defensive", "SP500")
drawdowns <- table.Drawdowns(bigT_ret)
drawdowns.dates <- cbind(format(drawdowns$From),format(drawdowns$To))
drawdowns.dates[is.na(drawdowns.dates)] <- format(index(bigT_ret)[NROW(bigT_ret)])
drawdowns.dates <- lapply(seq_len(nrow(drawdowns.dates)), function(i) drawdowns.dates[i,])


charts.PerformanceSummary(com_dd,ylog=FALSE,
                          period.areas = drawdowns.dates,
                          colorset = c(2,3,4),
                          legend.loc = "topleft",
                          main = "Log Comparison among Big Tech, Defensive, and SP500" )







chart.CumReturns(com_dd,ylog=FALSE,
                 period.areas = drawdowns.dates,
                 colorset = c(2,3,4),
                 legend.loc = "topleft",
                 main = "Cmulative")




# advanced charts.PerforanceSummary based on ggplot
gg.charts.PerformanceSummary <- function(rtn.obj, geometric = TRUE, main = "", plot = TRUE)
{
  
  # load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))
  
  # create function to clean returns if having NAs in data
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
    univ.rtn.xts.obj  
  }
  
  # Create cumulative return function
  cum.rtn <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
    y
  }
  
  # Create function to calculate drawdowns
  dd.xts <- function(clean.xts.obj, g = TRUE)
  {
    x <- clean.xts.obj
    if(g == TRUE){y <- PerformanceAnalytics:::Drawdowns(x)} else {y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)}
    y
  }
  
  # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
  cps.df <- function(xts.obj,geometric)
  {
    x <- clean.rtn.xts(xts.obj)
    series.name <- colnames(xts.obj)[1]
    tmp <- cum.rtn(x,geometric)
    tmp$rtn <- x
    tmp$dd <- dd.xts(x,geometric)
    colnames(tmp) <- c("Cumulative Return","Return","Drawdown") # names with space
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.POSIXct(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var="Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  # A conditional statement altering the plot according to the number of assets
  if(ncol(rtn.obj)==1)
  {
    # using the cps.df function
    df <- cps.df(rtn.obj,geometric)
    # adding in a title string if need be
    if(main == ""){
      title.string <- paste("Asset Performance")
    } else {
      title.string <- main
    }
    
    gg.xts <- ggplot(df, aes_string( x = "Date", y = "value", group = "variable" )) +
      facet_grid(variable ~ ., scales = "free_y", space = "fixed") +
      geom_line(data = subset(df, variable == "Cumulative Return")) +
      geom_bar(data = subset(df, variable == "Return"), stat = "identity") +
      geom_line(data = subset(df, variable == "Drawdown")) +
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      ggtitle(title.string) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
      ylab("") +
      xlab("")
    
  } 
  else 
  {
    # a few extra bits to deal with the added rtn columns
    no.of.assets <- ncol(rtn.obj)
    asset.names <- colnames(rtn.obj)
    df <- do.call(rbind,lapply(1:no.of.assets, function(x){cps.df(rtn.obj[,x],geometric)}))
    df$asset <- ordered(df$asset, levels=asset.names)
    if(main == ""){
      title.string <- paste("Asset",asset.names[1],asset.names[2],asset.names[3],"Performance")
    } else {
      title.string <- main
    }
    
    if(no.of.assets>5){legend.rows <- 5} else {legend.rows <- no.of.assets}
    
    gg.xts <- ggplot(df, aes_string(x = "Date", y = "value" )) +
      
      # panel layout
      facet_grid(variable~., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin = 
                   , labeller = label_value) + # label_value is default
      
      # display points for Cumulative Return and Drawdown, but not for Return
      geom_point(data = subset(df, variable == c("Cumulative Return","Drawdown"))
                 , aes(colour = factor(asset), shape = factor(asset)), size = 1.2, show.legend = TRUE) + 
      
      # manually select shape of geom_point
      scale_shape_manual(values = c(1,2,3)) + 
      
      # line colours for the Index
      geom_line(data = subset(df, variable == "Cumulative Return"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # bar colours for the Return
      geom_bar(data = subset(df,variable == "Return"), stat = "identity"
               , aes(fill = factor(asset), colour = factor(asset)), position = "dodge", show.legend = FALSE) +
      
      # line colours for the Drawdown
      geom_line(data = subset(df, variable == "Drawdown"), aes(colour = factor(asset)), show.legend = FALSE) +
      
      # horizontal line to indicate zero values
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      
      # horizontal ticks
      scale_x_datetime(breaks = date_breaks("3 months"), labels = date_format("%m/%Y")) +
      
      
      # main y-axis title
      ylab("") +
      
      # main x-axis title
      xlab("") +
      
      # main chart title
      ggtitle(title.string)
    
    # legend 
    
    gglegend <- guide_legend(override.aes = list(size = 3))
    
    gg.xts <- gg.xts + guides(colour = gglegend, size = "none") +
      
      # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
      # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
      
      theme( legend.title = element_blank()
             , legend.position = c(0,1)
             , legend.justification = c(0,1)
             , legend.background = element_rect(colour = 'grey')
             , legend.key = element_rect(fill = "white", colour = "white")
             , axis.text.x = element_text(angle = 0, hjust = 1)
             , strip.background = element_rect(fill = "white")
             , panel.background = element_rect(fill = "white", colour = "white")
             , panel.grid.major = element_line(colour = "grey", size = 0.5) 
             , panel.grid.minor = element_line(colour = NA, size = 0.0)
      )
    
  }
  
  assign("gg.xts", gg.xts,envir=.GlobalEnv)
  if(plot == TRUE){
    plot(gg.xts)
  } else {}
  
}

# display chart
gg.charts.PerformanceSummary(com_dd, geometric = TRUE)




#EWMA Function

EWMA<-function(x,lambda)

{

  returns<-Delt(x,type='log') ##calculate log returns

  return_sq<-returns^2 ##square log returns

  y<-as.matrix(x) ##convert from xts to matrix

  n=(1:nrow(y)-1) ##this will be used for the weights

  z<-as.matrix(n) ##convert from numeric to matrix

  weights<-(1 - lambda)*lambda^z ##calculating the weights

  weights<-sort(weights,decreasing=FALSE) #arrange weights from least to greatest as return data is arranged from oldest to newest

  product<-weights*return_sq

  ##multiply weights times squared log returns

  product<-as.matrix(product) ##convert to matrix

  product<-na.omit(product) ##remove all Nas in data

  Variance<-colSums(product) ##sum the product

  Volatility<-sqrt(Variance)

  final<-cbind(Variance,Volatility)

  ##combine columns of Variance and Volatility

}

a <- EWMA(port_price,.94) #test the function


start <- as.Date("2016-01-01")
end <- as.Date("2017-01-01")
diff <- as.numeric(endDate - maxDate)
a <- matrix(NA, diff, 2)


for (i in 1 : diff) {
  
  Appl_adj <- Ad(getSymbols("AAPL", auto.assign = F, src = "yahoo", from = start, to = end))
  MSFT_adj <- Ad(getSymbols("MSFT", auto.assign = F, src = "yahoo", from = start, to = end))
  Goog_adj <- Ad(getSymbols("GOOG", auto.assign = F, src = "yahoo", from = start, to = end))
  Csco_adj <- Ad(getSymbols("CSCO", auto.assign = F, src = "yahoo", from = start, to = end))
  
  port_price <- Appl_adj*0.25 + MSFT_adj*0.25 + Goog_adj*0.25 + Csco_adj*0.25
  
 
  a[i,1] <- EWMA(port_price,.94)[,1]
  a[i,2] <- EWMA(port_price,.94)[,2]
  start = start + 1
  end = end + 1
  print(paste(i,"Volatility",a[i,2]))
}




b <- matrix(NA, diff, 2)



for (i in 1 : diff) {
  
  
  Prt_adj <- Ad(getSymbols("DOC", auto.assign = F, src = "yahoo", from = start, to = end))
  Nee_adj <- Ad(getSymbols("NEE", auto.assign = F, src = "yahoo", from = start, to = end))
  Aep_adj <- Ad(getSymbols("AEP", auto.assign = F, src = "yahoo", from = start, to = end))
  JNJ_adj <- Ad(getSymbols("JNJ", auto.assign = F, src = "yahoo", from = start, to = end))
  
  

 
  port_price_def <- Prt_adj*0.25 + Nee_adj*0.25 + Aep_adj*0.25 +JNJ_adj*0.25
  
  b[i,1] <- EWMA(port_price_def,.94)[,1]
  b[i,2] <- EWMA(port_price_def,.94)[,2]
  
  
  start = start + 1
  end = end + 1
  print(paste(i,"Volatility",b[i,2]))
}


c <- matrix(NA, diff, 2)



for (i in 1 : diff) {
  
  
  SP500_adj <- Ad(getSymbols("^GSPC", auto.assign = F, src = "yahoo", from = start, to = end))
  
  ew
  c[i,1] <- EWMA(SP500_adj,.94)[,1]
  c[i,2] <- EWMA(SP500_adj,.94)[,2]
  
  
  start = start + 1
  end = end + 1
  print(paste(i,"Volatility",c[i,2]))
}

write.csv(c, file = "sp500.csv")

