### Problem 1
setwd ("K:/Downloads/FINANCIAL ECONOMETRICS/HW 3")
library(quantmod)
getSymbols("^GSPC",src="yahoo")
head(GSPC)
rtn <- dailyReturn(GSPC,type='log')
head(dr)
dr <- rtn
head(dr)
acf(dr) # check for serial correlations


# a,b. build MA model
m1a <- arima(dr,order=c(0,0,5)) 
m1a

tsdiag(m1a)
acf(m1a$residuals)

#c.d. Build AR model
y1 <- arima.sim(model=list(ar=c(.8,-.7)),1000) # <== simulate 1000 data points
acf(dr) # <== ACF shows dampening sine and cosine pattern.

m1b <- ar(y1,method="mle") # <== compute AIC criterion
m1b$order


m1c <- arima(dr,order=c(2,0,0)) 
m1c

acf(m1c$residuals)
tsdiag(m1c)

se <- sqrt(diag(m1c$var.coef)) # parameter standard errors
tval <- m1c$coef/se # parameter t-values
tval

#f. 
source("K:/Downloads/FINANCIAL ECONOMETRICS/backtest.R")

length(dr)

backtest(m1a,dr,50,h=1)
backtest(m1c,dr,50,h=1)

### Problem 2

da <- read.table ("K:/Downloads/FINANCIAL ECONOMETRICS/HW 3/m-PastorStambaugh.txt",header=T)
head(da)
dim(da)
xt<-(da[66:629,4])
head(xt)  
tail(xt)  

#a.Build time series model
m2a=ar(xt,method='mle')
m2a
m2a$order
acf(xt)
pacf(xt)

m2b <- arima(xt,order=c(10,0,0))
m2b 
tsdiag(m2b)
acf(m2b$residuals)

se <- sqrt(diag(m2b$var.coef)) # parameter standard errors
tval <- m2b$coef/se # parameter t-values
tval

c2<-c(0,NA,0,0,0,0,0,0,NA,NA,0)
m2c <- arima(xt,order=c(10,0,0),fixed=c2)
m2c
acf(m2c$residuals)
tsdiag(m2c)


#b. The model is not adeqaute.  tsdiag confirms inadequacy of the model


#c.Finding the largest outlier 

which.max(m2c$residuals)
i493<- rep(0,564)
plot(i493)
i493[493]<-1
plot(i493)


m2d <- arima(xt,order=c(10,0,0),xreg=c(i493))
m2d

tsdiag(m2d)


se <- sqrt(diag(m2d$var.coef)) # parameter standard errors
tval <- m2d$coef/se # parameter t-values
tval

c3<-c(0,0,0,0,0,0,0,0,NA,NA,NA,NA)
m2e <- arima(xt,order=c(10,0,0),fixed=c3,xreg=c(i493),transform.pars = FALSE)
m2e

tsdiag(m2e)
acf(m2e$residuals)




### Problem 3
da <- read.table("K:/Downloads/FINANCIAL ECONOMETRICS/HW 3/q-earn-cat83.txt",header=T)
head(da)

#a.Build time series model
eps <- da[,6]
epscorr <- eps + 1
head(epscorr)
xt <- log(epscorr)

acf(xt)
acf(diff(xt))
acf(diff(diff(xt),4))

m3a <- arima(xt,order=c(1,1,1)) # regression AR model for rate
m3a
tsdiag(m3a,gof=24)

acf(m3a$residuals) 

#b. Fit series to the seasonal model#

m5 <- arima(xt,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=4))
m5
tsdiag(m5,gof=24)

#c. Compare the models, which one is preferred?

#d.backtesting
source("K:/Downloads/FINANCIAL ECONOMETRICS/HW 3/backtest.R")
backtest(m8,eps,101,1)
backtest(m9,eps,101,1)






### Problem 4
da <- read.table("K:/Downloads/FINANCIAL ECONOMETRICS/HW 3/w-gasoil-9115.txt",header=T)
head(da)
dim(da)
yt <- da[,1]; xt <- da[,2]
m4a <- lm(yt~xt)
summary(m4a)

acf(m1$residuals)

dt <- diff(xt); ct <- diff(yt)
m4b <- lm(ct~dt)
summary(m4b)

acf(m2$residuals)
pacf(m2$residuals)

m4c <- lm(ct~dt-1)
summary(m4c)

acf(m4c$residuals)
pacf(m4c$residuals)

m4d <- ar(m4c$residuals,method="mle")
m4d$order

m4e <- arima(ct,order=c(11,0,0),xreg=dt,include.mean=F)
m4e
tsdiag(m4e,gof=24)

se <- sqrt(diag(m4e$var.coef)) # parameter standard errors
tval <- m4e$coef/se # parameter t-values
tval


fixed <- c(NA,0,NA,0,0,0,0,0,0,0,0,NA)
m4f <- arima(ct,order=c(11,0,0),xreg=dt,include.mean=F,fixed=fixed,transform.pars = FALSE)
m4f
tsdiag(m4f,gof=24) ### very close to that of the above AR(5) model

### Problem 5

#install.packages("fUnitRoots")

library(fUnitRoots)

help(UnitrootTests) # See the tests available

da <- read.table("K:/Downloads/FINANCIAL ECONOMETRICS/HW 3/w-gasoil-9115.txt",header=T)

m4g <- ar(yt,method="mle")
m4g$order

m4h <- ar(xt,method="mle")
m4h$order

adfTest(yt,lag=4,type="c") #Assume an AR(4) model for the series
adfTest(xt,lag=9,type="c")

# Cannot reject unit root

# A more careful analysis

x <- diff(gdp)

ord <- ar(x) # identify an AR model for the differenced series.
ord
