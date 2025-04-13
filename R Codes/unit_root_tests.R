# Unit root tests
# Example: 3-month treasury bill rate
# ===================================

library(quantmod)
getSymbols("TB3MS", src="FRED") 
plot(TB3MS)
rt <- TB3MS["1999-04/2020-01"]
length(rt)
plot(rt)
acf(rt)
mean(rt)

#------------------------------------------------
library(tseries)  # requires tseries package
?adf.test    
# Warning: adf.test() only runs the unrestricted regression,
# namely, it always includes a constant and a linear trend
adf.test(rt, k=0) # computes the DF test if setting the number of lags=0
adf.test(rt) # computes the ADF test
# The number of lags, k, used in the regression defaults to trunc((length(rt)-1)^(1/3)) 

#------------------------------------------------
library(urca) # requires urca package
# ADF test
?ur.df # performs the ADF test
result <- ur.df(rt, type="none") # lags=1 by default
summary(result) # For each of the above tests, use summary() to print the result
plot(result) # check residuals

result <- ur.df(rt, type="none", lags=0) # computes the DF test if setting lags=0
summary(result) # For each of the above tests, use summary() to print the result
plot(result) # check residuals

result <- ur.df(rt, type="drift", lags=6, selectlags="AIC") 
    # selects the number of lags by AIC, where max number of lags is set by lags
summary(result) # For each of the above tests, use summary() to print the result
plot(result) # check residuals
result <- ur.df(rt, type="drift", lags=6)
summary(result) # For each of the above tests, use summary() to print the result
plot(result) # check residuals
result <- ur.df(rt, type="trend", lags=6)
summary(result) # For each of the above tests, use summary() to print the result
plot(result) # check residuals


# KPSS test
?kpss.test # requires tseries package
kpss.test(rt, "Level")
?ur.kpss # requires urca package
result <- ur.kpss(rt, type="mu", lags="short")
summary(result)

# Phillips-Perron test
?ur.pp

# ADF-GLS test (or Elliott, Rothenberg & Stock test)
?ur.ers


library(forecast)
?ndiffs
ndiffs(rt, test="kpss", type="level")

drt <- diff(rt)[-1]
plot(drt)
result <- ur.df(drt, type="drift", selectlags="AIC")
summary(result)
