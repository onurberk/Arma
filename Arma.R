# ar(1) s�reci
set.seed(123)
y <- arima.sim(model=list(ar=0.75, ma=0.45), n=1000)
plot.ts(y)
summary(y)
#correlogram
acf(y, lag.max=20)
acf(y, lag.max=7, plot=F)
pacf(y, lag.max=20)
pacf(y, lag.max=7, plot=F)

LB.test <- lapply(1:24, function(i) Box.test(y, type="Ljung-Box", i))
LB.results <- data.frame(
  lag=1:24,
  xsquared=sapply(LB.test, "[[", "statistic"),
  pvalue=sapply(LB.test, "[[", "p.value")
)
head(LB.results, 24)

arma <- arima(y, order=c(0,0,0))
AIC(arma)

#tahmin
arma <- arima(y, order=c(1,0,1))
arma

#kontrol
#a - overfit
arma <- arima(y, order=c(2,0,1))
arma

arma <- arima(y, order=c(1,0,2))
arma

#b - artiklar
arma <- arima(y, order=c(1,0,1))

res <- residuals(arma)
acf(res, lag.max=20)
pacf(res, lag.max=20)



Box.test(res, lag=1, "Ljung-Box")

LB.test <- lapply(1:24, function(i) Box.test(res, type="Ljung-Box", i))
LB.results <- data.frame(
  lag=1:24,
  xsquared=sapply(LB.test, "[[", "statistic"),
  pvalue=sapply(LB.test, "[[", "p.value")
)
head(LB.results, 24)

install.packages("aTSA")
library(aTSA)

identify(y, p=3, q=3, nlag=3,intercept=T, 
         stat.test=F, output=T)


a <- read.csv("C:/Users/Onur/OneDrive/Masaüstü/Kodlar/R/Proje -I/xu100_week.csv")

summary(a)

xu100.ts <- ts(a$xu100)
plot.ts(xu100.ts)
acf(xu100.ts, lag.max=20)
acf(xu100.ts, lag.max=20, plot=F)
pacf(xu100.ts, lag.max=20)

rbist <- 100*diff(xu100.ts, lag=1, diff=1)/lag(xu100.ts,-1)

plot.ts(rbist)
acf(rbist, lag.max=20)
pacf(rbist, lag.max=20)

LB.test <- lapply(1:24, function(i) Box.test(rbist, type="Ljung-Box", i))
LB.results <- data.frame(
  lag=1:24,
  xsquared=sapply(LB.test, "[[", "statistic"),
  pvalue=sapply(LB.test, "[[", "p.value")
)
head(LB.results, 24)