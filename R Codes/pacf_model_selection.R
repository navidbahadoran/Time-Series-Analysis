# Examine the PACF of the following processes
# ===========================================
# AR(1)
set.seed(100)
y<-arima.sim(n=1000, list(ar=0.7))
plot.ts(y)
acf(y)
(pacf(y))

# AR(2)
set.seed(100)
y <-arima.sim(n=1000, list(ar=c(0.6,0.2)))
plot.ts(y)
acf(y)
(pacf(y))

# MA(1)
set.seed(100)
y <-arima.sim(n=1000, list(ma=0.7))
plot.ts(y)
acf(y)
pacf(y)

# ARMA(1,1)
set.seed(100)
y <-arima.sim(n=1000, list(ar=0.7, ma=0.5))
plot.ts(y)
acf(y)
pacf(y)

# an alternative useful function
library(forecast)
tsdisplay(y) # plot time series, ACF, and PACF together


# Model selection
# ===============
# Suppose the true DGP is an ARMA(1,1)
set.seed(100)
y <-arima.sim(n=1000, list(ar=0.7, ma=0.5))
# Fit an ARMA(1,1) model
(fit_arma11 <- arima(y, order=c(1,0,1))) 

fit_arma11$aic
AIC(fit_arma11)
-2*fit_arma11$loglik + 2*4  # verify that we can obtain the same AIC value

BIC(fit_arma11)
-2*fit_arma11$loglik + log(1000)*4 

# Other candidate models
(fit_ma1 <- arima(y, order=c(0,0,1)))
(fit_ar1 <- arima(y, order=c(1,0,0)))

AIC(fit_ar1, fit_ma1, fit_arma11)
BIC(fit_ar1, fit_ma1, fit_arma11)

# Fit the best ARIMA model automatically
?auto.arima  # requires forecast package
auto.arima(y)  

# Suppose now the true DGP is an ARMA(2,2)
set.seed(100)
arma22 <- arima.sim(n=1000, list(ar=c(0.5,-0.25), ma=c(0.6,-0.3)))

auto.arima(arma22)
auto.arima(arma22, seasonal = FALSE) # Note that seasonal defaults to TRUE
auto.arima(arma22, max.p=4, max.q=4, start.p=1, start.q=1, d=0, seasonal=FALSE)
# default: max.p=5, max.q=5, start.p=2, start.q=2, d=NA
auto.arima(arma22, ic=c("bic")) 
# ic=c("aicc","aic", "bic"), defaults aicc
auto.arima(arma22, trace=TRUE)  # set trace=TRUE to see the selection process

fit_arma22<-auto.arima(arma22)
checkresiduals(fit_arma22)

# An illustration of the auto.arima() algorithm can be found in 
# https://otexts.com/fpp3/arima-r.html
# See the following article for a discussion on the success rates of automated 
# ARIMA fitting.
# http://freerangestats.info/blog/2015/09/30/autoarima-success-rates

