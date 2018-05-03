library(quantmod)
library(fBasics) 

#scartch the data
getSymbols("^GSPC",from="2010-6-1",to="2016-6-3")
head(GSPC)
tail(GSPC)#test
windows()
chartSeries(GSPC)      
addTA(SMA(Cl(GSPC),n=20), on=2, col="blue")

#obtain periodly returns
drt=dailyReturn(GSPC)
mrt=monthlyReturn(GSPC)
yrt=annualReturn(GSPC)
ldrt=dailyReturn(GSPC,type="log")
lmrt=monthlyReturn(GSPC,type="log")
lyrt=annualReturn(GSPC,type="log")
basicStats(drt)
basicStats(mrt)
par(mfcol=c(2,2)) 
plot(drt,type='l',ylab='Return',main='Daily returns of S&P 500: 2006 to 2016')
plot(mrt,type='l',ylab='Return',main='Monthly returns of S&P 500: 2006 to 2016')
plot(ldrt,type='l',ylab='Log Return',main='Daily log returns of S&P 500: 2006 to 2016')
plot(lmrt,type='l',ylab='Log Return',main='Monthly log returns of S&P 500: 2006 to 2016')

#density
par(mfcol=c(2,2)) ##<== To put two plots on a single page
ydrt=drt*100 ##<== percentage returns
hist(ydrt,nclass=50,xlim=c(-10,10),xlab = 'daily returns*100',col='red',main='Histogram of daily returns')
ddens=density(ydrt,from=-10,to=10)
plot(ddens$x,ddens$y,xlab='daily returns*100',ylab='density',type='l',col='blue',main='Density of daily returns')
ymrt=mrt*100 ##<== percentage returns
hist(ymrt,nclass=50,xlim=c(-10,10),xlab='monthly returns*100',col='red',main='Histogram of monthly returns')
mdens=density(ymrt,from=-10,to=10)
plot(mdens$x,mdens$y,xlab='monthly returns*100',ylab='density',type='l',col='blue',main='Density of monthly returns')



#test distribution(daily)
t.test(drt)
normalTest(drt,method='jb') ##<=== Test for the normality assumption
ds3=skewness(drt) ##<== Perform skewness test
dT=length(drt)
dtst=ds3/sqrt(6/dT)
dpv=2*pnorm(dtst) ##<== Compute two-sided p-value of the test statistic
print(dpv)
dk4=kurtosis(drt) ##<== Perform kurtosis test
dtst2=dk4/sqrt(24/dT)
dpv2=2*pnorm(dtst2)
print(dpv2)

#test distribution(monthly)
t.test(mrt)
normalTest(mrt,method='jb') ##<=== Test for the normality assumption
ms3=skewness(mrt) ##<== Perform skewness test
mT=length(mrt)
mtst=ms3/sqrt(6/mT)
mpv=2*pnorm(mtst) ##<== Compute two-sided p-value of the test statistic
print(mpv)
mk4=kurtosis(mrt) ##<== Perform kurtosis test
mtst2=mk4/sqrt(24/mT)
mpv2=2*pnorm(mtst2)
print(mpv2)

#ACF
par(mfcol=c(2,1))
acf(drt,lag=100,xlab='interval',ylab='Autocorrelation',main='ACF of daily returns') 
acf(ldrt,lag=100,xlab='interval',ylab='Autocorrelation',main='ACF of daily log returns') 
par(mfcol=c(2,1))
acf(mrt,lag=12,xlab='interval',ylab='Autocorrelation',main='ACF of monthly returns') 
acf(lmrt,lag=12,xlab='interval',ylab='Autocorrelation',main='ACF of monthly log returns') 

Box.test(drt,lag=100)
Box.test(drt,lag=100,type='Ljung') 
Box.test(mrt,lag=12) 
Box.test(mrt,lag=12,type='Ljung') 


