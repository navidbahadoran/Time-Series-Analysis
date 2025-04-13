# Estimating ARMA models
# ======================

# Simulate an AR(2) process
set.seed(450)
y <- arima.sim(n=100, list(ar=c(0.6,0.2))) + 5
plot.ts(y)
acf(y)


?arima # check out the help documentation
arima(y, order=c(2,0,0), method="ML")
arima(y, order=c(2,0,0), method="CSS")
arima(y, order=c(2,0,0), method="CSS-ML") # default, can be abbreviated
arima(y, order=c(2,0,0), include.mean = FALSE) # defaults to TRUE
# NOTE: The mean of y is called "intercept" in the arima() output!
# Recall that the intercept term in an AR(2) model relates to mu by 
# intercept = mu*(1-phi1-phi2).

# Fit an MA(1) model
fit_ma1 <- arima(y, order=c(0,0,1)) 
fit_ma1

# Fit an ARMA(1,1) model
(fit_arma11 <- arima(y, order=c(1,0,1))) 

# Fit an AR(2) model
(fit_ar2 <-arima(y, order=c(2,0,0)))

ts.plot(y, gpars = list(col = "black"))
lines(fitted(fit_ar2), col = "red", lty = 2)

# Extract results (if needed)
fit_ar2$coef
fit_ar2$sigma2
fit_ar2$var.coef


# Residual diagnostics
# ====================
resid <- fit_ar2$residuals
plot.ts(resid)
acf(resid)
Box.test(resid, type="Ljung-Box", lag=10, fitdf=2) 
# We need to correct the degrees of freedom since the test is applied to residuals.
# fitdf=p+q: number of degrees of freedom to be subtracted


#install.packages("forecast")
library(forecast)
checkresiduals(fit_ar2)
# A helpful function that produces time plot, ACF plot and histogram of the residuals 
# (overlaid with a normal distribution for comparison), and does the Ljung-Box test
# with correct degrees of freedom.

checkresiduals(fit_ma1$residuals)
checkresiduals(fit_arma11$residuals)
# It may not be able to find the appropriate degrees of freedom.


plot(fit_ar2)
autoplot(fit_ar2)
autoplot(fit_arma11)


# Try the alternative ar() function and compare different methods
# ================================================================
# ar() is a wrapper for ar.yw, ar.ols, ar.mle...
?ar
ar(y, aic=FALSE, order.max=2, method="yule-walker") # by default demean = TRUE
ar(y, aic=FALSE, order.max=2, method="ols")
ar(y, aic=FALSE, order.max=2, method="mle")
ar(y, order.max=2) # by default selects the complexity by AIC and uses "yule-walker"
# Note: The true intercept, not mean of y, is reported as "intercept" in the ar() output.
# Compare the ar(..., method="ols") method with arima(..., method="CSS")


# Also check out the Arima() function from the forecast package, 
# which is an improved wrapper for arima().
Arima(y, order=c(2,0,0))
