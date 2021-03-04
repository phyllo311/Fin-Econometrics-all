# Week 2: 
# EX 1: Consider the daily VIX index of CBOE. Download data.

setwd("home_path")
library(quantmod)
getSymbols("^VIX",src="yahoo")
head(VIX)

VIX = as.double(VIX$VIX.Adjusted) # Transform into vector, as.numeric()

#a. Plot daily VIX and its logs series on same page
logVIX = log(VIX)
par(mfcol=c(2,1))
plot(ts(VIX,start=c(2007,1),frequency=265),main="Daily VIX index",xlab='year',ylab='VIX')
plot(ts(logVIX,start=c(2007,1),frequency=265),main="Daily log VIX index",xlab='year',ylab='log VIX')

#b. unit root
require(fUnitRoots)
m1 = ar(diff(logVIX),method="mle")
m1$order
adfTest(logVIX,lags=11,type="nc")

#c. tst mean of growth rate
zt = diff(logVIX)
mean(zt)
t.test(zt)

#d. compute ACF and PACFof zt. Plot on same page
par(mfcol=c(2,1))
acf(zt)  
pacf(zt)

#e. Test autocorrelation
Box.test(zt,lag=10,type="Ljung")


#2. Consider growth rate zt in #1 
#a. Identify AR model for zt. Fit the specifed AR model, perform
#model checking, and write down the fitted model.

m1 = ar(zt,lag.max=20,method="mle")
names(m1)
m1$order
m1

t.test(zt)  ### Checking mean value

m2 = arima(zt,order=c(10,1,0))
m2
tsdiag(m2,gof=24)

c1 = c(NA,NA,NA,NA,NA,0,NA,0,0,NA)  ### Remove insignificant parameters

m2a = arima(rate,order=c(10,1,0),fixed=c1)
m2a
tsdiag(m2a,gof=24)

#b. Identify AR model for y = log(vt). Compute 1-step to 3-step ahead point forecasts
yt = logVIX
m3 = arima(yt,order=c(10,1,0)) # regression AR model for rate
m3
tsdiag(m3,gof=24)

m3p = predict(m3,3) # Prediction
names(m3p)

#c. Compute 1-step to 3-step ahead 95% CI for yt
lcl = m3p$pred-1.96*m3p$se  # <=== calculate lower 95% interval
ucl = m3p$pred+1.96*m3p$se  # <=== calculate upper 95% interval
lcl
ucl

#3. Consider log VIX index yt in #2
#a. Fit Arima(1,1,1) model to yt
m4 = arima(yt,order=c(1,1,1))
m4
tsdiag(m4,gof=24)

#b. Check if model adequate. Which residual ACF is significantly different from zero.
tsdiag(m4,gof=24)
par(mfrow = c(1,1))
acf(m4$residuals)

#c. Fit model given command below and perform backtest. Initial forecast origin at t = 2200
mm = arima(yt,order=c(1,1,1),seasonal=list(order=c(0,0,1),period=10))
mm # <== see parameter estimates
names(mm) # <== See the output 
tsdiag(mm,gof=20)  # Model checking
source("K:/Downloads/FINANCIAL ECONOMETRICS/Lec 2/backtest.R")
backtest(m3,yt,2200,1)
backtest(mm,yt,2200,1)

# 4. Data ICSA taken from FRED contains weekly jobless claims. 
# The series is seasonally adjusted. See FED FRED. Let yt be the series 
#of initial jobless claims.

getSymbols("ICSA",src="FRED")
head(ICSA)
ICSA = as.double(ICSA$ICSA)
yt = ICSA

#a. Does yt series have unit root?
par(mfcol=c(1,1))
plot(ts(yt,start=c(1967,1),frequency=52),main="Weekly jobless claim",xlab='year',ylab='%')
acf(yt)
n1 = ar(diff(yt),method="mle") # check unit root using the first diff
n1$order
adfTest(yt,lags=10,type="nc")

#b.Focus on change series of the claims rt, ie. the first differenced series. Is rt serially correlated?
rt = diff(yt)
Box.test(rt,lag=10,type="Ljung")

#c. Build AR model for rt series. Perform model checking using god = 24. Is model adequate?
m = ar(rt,lag.max=20,method="mle") # regress AR model for rt
names(m1)
m$order
m

m1 = arima(rt,order=c(9,0,0)) # regression AR model for rt
m1
tsdiag(m1,gof=24)

#d.Refine the AR model by removing any estimate with t-ratio less than
# 1.65 in absolute value (t-ratio is obtained by dividing the estimated
# parameter value by its standard error s.e.) Write down the refined
# model. Is it adequate? Why? Denote the refined model by m2.

c1 = c(NA,NA,NA,0,0,0,0,0,NA,0)  ### Remove insignificant parameters

m2 = arima(rt,order=c(9,0,0),fixed=c1)
m2
tsdiag(m2,gof=24)


#e. Back test with forecast origin t = 2500 to compare models m1 and m2
source("K:/Downloads/FINANCIAL ECONOMETRICS/Lec 2/backtest.R")
length(rt)
backtest(m1,rt,2500,1)
backtest(m2,rt,2500,1)

#f.To obtain the forecasts of the claims, we like to use the yt series
#directly. Since rt = yt - y(t-1), an ARIMA(p; 0; q) for rt implies
#an ARIMA(p; 1; q) for yt. Refit a model for yt. Compare the estimates of
#AR coefficients with those of the model for rt. Is there any difference?

mm = ar(yt,lag.max=20,method="mle") # regress AR model for rt
names(mm)
mm$order
mm

m3 = arima(yt,order=c(10,1,0)) # regression AR model for yt
m3
tsdiag(m3,gof=24)


c2 = c(NA,NA,NA,0,0,0,0,0,NA,0)  ### Remove insignificant parameters

m4 = arima(yt,order=c(10,1,0),fixed=c2)
m4
tsdiag(m4,gof=24)


#g. Based on the model for yt, obtain 1-step to 3-step ahead forecasts for
#yt at the forecast origin equal to the last observation.
predict(m4,3)
