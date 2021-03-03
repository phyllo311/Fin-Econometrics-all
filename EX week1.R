setwd("path")
library(fBasics)
#Assignment 1
# Consider the daily simple returns of Google (GOOG) stock, CRSP value-weighted index (VW), 
# CRSP equal-weighted index (EW), and the S&P composite index (SP) from January 3, 2011 to December 31, 2014. 
# Returns of the three indices include dividends. The data are in file d-goog3dx.txt 
# and the columns show permno of GOOG, date, GOOG, vw, ew, and sp, respectively, with the last four columns 
# showing the simple returns.

#(a) Compute the sample mean, standard deviation, skewness, excess kurtosis, minimum, 
#and maximum of each simple return series.
da <- read.table("d-goog3dx.txt",header=T)
da[1,]

goog <- da[,3]
vw <- da[,4]
ew <- da[,5]
sp <- da[,6]

basicStats(goog)
basicStats(vw)
basicStats(ew)
basicStats(sp)

#(b) Obtain the empirical density function of the simple returns of Google. 
#Are the daily simple returns normally distributed? Why?
#Perform a normality test to justify your answer.
d1 <- density(goog)
names(d1)
plot(d1$x,d1$y,xlab='Simple Return',ylab='Density',main='Google stock',type='l')
normalTest(goog,method="jb")

#(c) Transform the simple returns to log returns. 
#Compute the sample mean, std, skewness, excess kurtosis, minimum, and maximum of each log return series.
lgoog=log(goog+1)
lvw=log(vw+1)
lew=log(ew+1)
lsp=log(sp+1)

basicStats(lgoog)
basicStats(lvw)
basicStats(lew)
basicStats(lsp)

#(d) Test the null hypothesis that the mean of the log returns of Google stock is zero.
t.test(lgoog)                             

#(e) Obtain the empirical density plot of the daily log returns of Google stock and the S&P composite index.
d2 <- density(lgoog)
names(d2)
plot(d2$x,d2$y,xlab='Log Return',ylab='Density',main='Google Stock',type='l')

d3 <- density(lsp)
names(d3)
plot(d3$x,d3$y,xlab='Log Return',ylab='Density',main='SP Composite',type='l')

# Assignment 3
# Consider the monthly log returns of PG stock from January 1961 to Dec 2014.
#Data file is m-pg3dx-6114.txt with column names PERMNO of PG, date, ge, vwretd, ewretd, and sprtrn, respectively.
# Perform the tests and draw conclusions using the 5% signicance level.
# (a) Construct a 95% confidence interval for the monthly log returns of PG stock.
da <- read.table("m-pg3dx-6114.txt",header=T)
da[1,]
pg <- da[,3]
basicStats(pg)
lpg=log(pg+1)
basicStats(lpg)
t.test(lpg)

# (b) Test H0 : m3 = 0 versus Ha : m3 6= 0, where m3 denotes the skewness of the return.
t1 = skewness(lpg)/sqrt(6/length(lpg))
t1
pv=2*pnorm(t1)
pv

# (c) Test H0 : K = 3 versus Ha : K 6= 0, where K denotes the kurtosis.
k4 <- kurtosis(lpg)
t2 = kurtosis(lpg)/sqrt(24/length(lpg))
t2

# Assignment 4
# Consider the daily log returns of Google stock from January 3, 2011 to
# December 31, 2014 as in Problem 1. Perform the following tests:
#  (a) Test the null hypothesis that the log return is symmetric with respect
#to its mean;

t1 <- skewness(lgoog)/sqrt(6/length(lgoog))
t1

# (b) Test the null hypothesis that the excess kurtosis of the returns is zero;
k5 <- kurtosis(lgoog)
t2 = kurtosis(lgoog)/sqrt(24/length(lgoog))
t2

# (c) Construct a 95% confidence interval for the expected daily log return of Google stock.
t.test(lgoog)

# Assignment 5. 
#Daily foreign exchange rates (spot rates) can be obtained from the Federal
#Reserve Bank in St Louis (FRED). The data are the noon buying rates
#in New York City certied by the Federal Reserve Bank of New York.
#Consider the exchange rates between the U.S. dollar and the UK Sterling
#from January 3, 2006 to March 20, 2015. See the le d-exusuk.txt.
#The file has four columns, namely year, month, day, and fx, respectively.
#Answer the following questions:

#(a) Compute the daily log return of the exchange rate.
y <- read.table("d-exusuk-0615.txt",header=T) # <== Load USEU exchange rates
dim(y)
y[1,]
r <- diff(log(y[,4]))

#(b) Compute the sample mean, standard deviation, skewness, excess kurtosis, 
#minimum, and maximum of the log returns of the exchange rate.
basicStats(r)

#(c) Obtain a density plot of the daily long returns of Dollar-Pound exchange rate.
d7 <- density(r)
names(d7)
plot(d7$x,d7$y,xlab='Log Returns',ylab='Density',main='Density Plot of log returns of Dollar-Pound exchange rate',type='l')

#(d) Test H0 : m = 0 versus Ha : m # 0, where m denotes the mean of the
# daily log return of Dollar-Pound exchange rate.
t.test(r)
