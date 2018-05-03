library(xts)
startdate = c(2014,6,3)

# read in data in order to calculate volatility
da<-read.table("SP500.txt")
head(da)
tail(da)
dim(da)

da.xts<-as.xts(da)
spx.xts<-da.xts$Adj.Close    #extract stock price
plot(spx.xts, main = "the S&P500 index", xlab = 'time')

rtd.SP<-diff(log(spx.xts))*100
rtd.SP<-rtd.SP[-1,]
rtd.SP<-rtd.SP['2014-06-03/']
plot(rtd.SP, main = "the log return of S&P500 index", xlab = 'time')
Box.test(rtd.SP, lag=12, type='Ljung-Box') # significant autocorrelation
Box.test(rtd.SP^2, lag=12, type='Ljung-Box') # no autocorrelation

FinTS::ArchTest(x=rtd.SP, lags=12)  # significant ARCH effect

##### Use fGarch package
library(fGarch)
GARCH.model_1 <- garchFit(~garch(1,1), data=rtd.SP, trace=FALSE)
GARCH.model_2 <- garchFit(~garch(1,1), data=rtd.SP,  cond.dist='std', trace=FALSE)

#plot(GARCH.model_1)

vol_1 <- fBasics::volatility(GARCH.model_2) 
res_1 <- residuals(GARCH.model_2, standardize=TRUE)
vol_1.ts <- ts(vol_1, frequency=252, start=startdate)
res_1.ts <- ts(res_1, frequency=252, start=startdate)
par(mfcol=c(2,1))
plot(vol_1.ts, xlab='Time', ylab='Volitility')
plot(res_1.ts, xlab='Time', ylab='Residuals')
     
### Use ruGarch package
require(rugarch)
spec=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), # try eGarch Model
                mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),
                distribution.model = "norm",fixed.pars=list(mu=0))
myfit=ugarchfit(spec,data=rtd.SP,solver="solnp")
myfit

### obtain volatility
vol <- sigma(myfit)^2
vol.ts<-ts(vol, frequency = 252, start = startdate)
par(mfcol=c(1,1))
plot(vol.ts, xlab='Time', ylab='Volitility', main = "The volitility Distribution using Garch Model")
par(new=T)
plot(vol_1.ts, xlab='Time', ylab='Volitility', col=3, ylim = c(min(vol.ts),max(vol.ts)))
legend("topleft", c('eGarch','sGarch'), col = c(1,3), lty = c(1, 1), bty = "n")

mean(vol.ts) # the volatility from eGarch model
mean(vol_1.ts) # the volatility from standard Garch model

#forecast
#fore = ugarchforecast(myfit, n.ahead = 20)
#show(fore)
#plot(fore)


##########
#
#  option pricing
#
##########
fre = 252
expiredate = 90-3

SP.SP500<-spx.xts['2016-06-03']

OP.SP500<-read.csv("SP500OPL.csv")
OP.strick<-OP.SP500$Strike
OP.C.Ask<-OP.SP500$Ask
OP.C.Bid<-OP.SP500$Bid
OP.P.Ask<-OP.SP500$Ask.1
OP.P.Bid<-OP.SP500$Bid.1
n<-nrow(OP.SP500)

require(fOptions)

P<-as.numeric(SP.SP500)
T=expiredate/365
sigma=mean(vol_1.ts)/100
r=0.018

CP<-matrix(NA,2,n)
for (i in 1:n) {
  K<-OP.strick[i]
  CP[1,i]<-GBSOption(TypeFlag = "c", S = P, X = K,  #calculate call price
                     Time = T, r = r,b = r, sigma = sigma)@price 
  CP[2,i]<-GBSOption(TypeFlag = "p", S = P, X = K,  #calculate put price
                     Time = T, r = r,b = r, sigma = sigma)@price
  }

#Draw the comparsion bewteen using BS formula and the actual value
par(mfcol=c(1,1))   # Call Option
plot(OP.strick,OP.C.Ask, xlab='Strick Price', ylab='Stock Price', 
     main = "BS Call Option", type = "l", ylim = c(0,2000), lwd=2, col=1, pch=24)
par(new=T)
plot(OP.strick,CP[1,], col=3,xlab='Strick Price', ylab='Stock Price', 
     type = "l", ylim = c(0,2000), lwd=2, pch=24)
abline(v=P, col='red', lty=3)
legend("top", c('BS formula','call option Ask'), col = c(3,1), lty = c(1, 1), bty = "n")

par(mfcol=c(1,1))   # Put Option
plot(OP.strick,OP.P.Ask, xlab='Strick Price', ylab='Stock Price', 
     main = "BS Put Option", type = "l", ylim = c(0,2000), lwd=2, col=1, pch=24)
par(new=T)
plot(OP.strick,CP[2,], col=3,xlab='Strick Price', ylab='Stock Price', 
     type = "l", ylim = c(0,2000), lwd=2, pch=24)
abline(v=P, col='red', lty=3)
legend("top", c('BS formula','put option Ask'), col = c(3,1), lty = c(1, 1), bty = "n")

###########
#
# Calculate implied Volatility
#
##########

C.vol.Ask <- numeric(n)
C.vol.Bid <- numeric(n)
P.vol.Ask <- numeric(n)
P.vol.Bid <- numeric(n)

for(i in 1:n){
  K<-OP.strick[i]
  if(!is.na(OP.C.Ask[i])){
    C.vol.Ask[i] <- GBSVolatility(price=OP.C.Ask[i], TypeFlag='c', S=P, X = K, Time=T, r=r, b=r)
    C.vol.Bid[i] <- GBSVolatility(price=OP.C.Bid[i], TypeFlag='c', S=P, X = K, Time=T, r=r, b=r)
  }

  if(!is.na(OP.P.Ask[i])){
    P.vol.Ask[i] <- GBSVolatility(price=OP.P.Ask[i], TypeFlag='p', S=P, X = K, Time=T, r=r, b=r)
    P.vol.Bid[i] <- GBSVolatility(price=OP.P.Bid[i], TypeFlag='p', S=P, X = K, Time=T, r=r, b=r)
  }
}

CC<-matrix(NA,n,8)
for (jj in 1:n) {
  if(C.vol.Ask[jj]>=0.01){
    CC[jj,1]<-C.vol.Ask[jj]
    CC[jj,2]<-OP.strick[jj]
  }
  if(C.vol.Bid[jj]>=0.01){
    CC[jj,3]<-C.vol.Bid[jj]
    CC[jj,4]<-OP.strick[jj]
  }
  if(P.vol.Ask[jj]>=0.01){
    CC[jj,5]<-P.vol.Ask[jj]
    CC[jj,6]<-OP.strick[jj]
  }
  if(P.vol.Bid[jj]>=0.01){
    CC[jj,7]<-P.vol.Bid[jj]
    CC[jj,8]<-OP.strick[jj]
  }
}

par(mfrow=c(1,2))
plot(c(OP.strick, OP.strick), c(C.vol.Bid, C.vol.Ask), typ='n', main='Volatility Smile for Call', xlab='Strick Price', ylab='Volatility')
lines(CC[,2],CC[,1], lty=1)
lines(CC[,4],CC[,3], lty=2)
abline(v=P, col='red', lty=3)
legend('bottomleft', c('Ask', 'Bid'), lty=c(1,2))

plot(c(OP.strick, OP.strick), c(P.vol.Bid, P.vol.Ask), typ='n', main='Volatility Smile for Put', xlab='Strick Price', ylab='Volatility')
lines(CC[,6],CC[,5], lty=1)
lines(CC[,8],CC[,7], lty=2)
abline(v=P, col='red', lty=3)
legend('bottomleft', c('Ask', 'Bid'), lty=c(1,2))


# (3) show results
par(mfrow=c(1,2))
plot(c(OP.strick, OP.strick), c(C.vol.Bid, C.vol.Ask), typ='n', main='Volatility Smile for Call', xlab='Strick Price', ylab='Volatility')
lines(OP.strick, C.vol.Bid, lty=1)
lines(OP.strick, C.vol.Ask, lty=2)
abline(v=P, col='red', lty=3)
legend('bottomleft', c('Ask', 'Bid'), lty=c(1,2))

plot(c(OP.strick, OP.strick), c(P.vol.Bid, P.vol.Ask), typ='n',  main='Volatility Smile for Put', xlab='Strick Price', ylab='Volatility')
lines(OP.strick, P.vol.Bid, lty=1)
lines(OP.strick, P.vol.Ask, lty=2)
abline(v=P, col='red', lty=3)
legend('bottomleft', c('Ask', 'Bid'), lty=c(1,2))

