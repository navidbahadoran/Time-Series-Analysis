# A VAR model of GDP growth rate and term spread
# ==============================================
library(quantmod)
library(urca)
library(vars)

# Retrieve quarterly real GDP
gdp <- getSymbols("GDPC1", src="FRED", auto.assign=FALSE)
plot(gdp)
gdpgr <- diff(log(gdp))*100  # real GDP growth rate
plot(gdpgr)

# Retrieve monthly short-term and long-term interest rates
getSymbols("TB3MS", src="FRED") # 3-Month Treasury Bill: Secondary Market Rate
getSymbols("GS10", src="FRED") # 10-Year Treasury Constant Maturity Rate
plot(merge(TB3MS, GS10), main = "Interest Rates")
addLegend("topright", legend.names = c("TB3MS", "GS10"),
          lty=c(1,1), bg="white", bty="o")


spread <- GS10 - TB3MS  # term spread
spread_q <- apply.quarterly(spread, mean)  
    # compute quarterly spread by taking monthly averages
time(spread_q) <- as.Date(as.yearqtr(time(spread_q)))  
    # set first month of each quarter as the index

data <- merge(gdpgr, spread_q)
colnames(data) <- c("GDP_growth", "Term_spread")

y <- data["1981/2019"] # select sample
plot(y)

ur_gdp <- ur.df(y$GDP_growth, type="drift", lags=4, selectlags="AIC")
summary(ur_gdp)
ur_spread <- ur.df(y$Term_spread, type="drift", lags=4, selectlags="AIC")
summary(ur_spread)


# Examine cross-correlation matrices
MTS::ccm(y) # requires MTS package (MTS also contains a function called VAR, but
            # I will use the VAR function from the vars package.)
# The meaning of the simplified notations (Tiao and Box, 1981):
#       + indicates correlation coefficient >= 2/sqrt(T)
#       - indicates correlation coefficient <= 2/sqrt(T)
#       . otherwise

MTS::ccm(y, level=T) # print numerical values of CCM

# Compute the multivariate Ljung-Box Q statistics (adj=0 by default)
MTS::mq(y, lag=8) 

# Order selection by information criteria
VARselect(y, type="const") # BIC is named "SC" (Schwarz criterion)

VAR_fit <- VAR(ts(y), p=2, type="const") # can choose lag order by "ic=" (but  
                                        # must set "lag.max" simultaneously)
summary(VAR_fit)
roots(VAR_fit, modulus=FALSE) # eigenvalues of the companion coefficient matrix
roots(VAR_fit) # modulus defaults to TRUE

# Diagnostic checking
res <- serial.test(VAR_fit, lags.pt=8, type="PT.adjusted")
res
plot(res)

plot(res, names="GDP_growth")
plot(res, names="Term_spread")

# Forecasting
(fcast <- predict(VAR_fit, n.ahead=8))
plot(fcast)
fanchart(fcast)

# Granger causality
causality(VAR_fit, cause="GDP_growth")
causality(VAR_fit, cause="Term_spread")

# Impulse response function (by default, ortho = TRUE)
irf_gdp <- irf(VAR_fit, impulse="Term_spread", response="GDP_growth", n.ahead=20)
plot(irf_gdp, ylab="GDP growth")
irf_spread <- irf(VAR_fit, impulse="GDP_growth", response="Term_spread", n.ahead=20)
plot(irf_spread, ylab="Term spread")
plot(irf(VAR_fit)) # examine all four IRFs

# Forecast error variance decomposition
fevd_result <- fevd(VAR_fit, n.ahead=12)
fevd_result
plot(fevd_result)


# {vars} reference manual
# https://cran.r-project.org/web/packages/vars/vars.pdf
# vignettes
# http://www2.uaem.mx/r-mirror/web/packages/vars/vignettes/vars.pdf
