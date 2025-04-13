# Estimate GARCH models
# =====================
library(quantmod)
library(forecast)
library(rugarch)

MSFT_full <- getSymbols("MSFT", src="yahoo", auto.assign = F)
plot(MSFT_full$MSFT.Close)
MSFT <- MSFT_full["2017/2019"]
head(MSFT)
tail(MSFT)
r <- diff(log(MSFT$MSFT.Close))[-1]
length(r)
plot(r, main="Daily returns")
plot(r^2, main="Daily squared returns")


tsdisplay(r)
Box.test(r, lag = 10, type="Ljung-Box")

tsdisplay(r^2)
Box.test(r^2, lag = 10, type="Ljung-Box")

library(FinTS)
ArchTest(r, 10)  # Engle's ARCH LM test
# This is just an illustration. Should apply ARCH tests to the residuals of 
# a conditional mean model.


# Fit an ARCH(1) model
# ugarchspec defines the GARCH order by (q,p) (ARCH, GARCH)
spec_arch1 <- ugarchspec(variance.model = list(garchOrder=c(1,0)), 
                         mean.model = list(armaOrder=c(0,0)))
fit_arch1 <- ugarchfit(spec=spec_arch1, data=r) # Fails to converge using default solver
fit_arch1 <- ugarchfit(spec=spec_arch1, data=r, solver ='hybrid')
fit_arch1
plot(fit_arch1)
plot(fit_arch1, which=11) # if you want to select option 11


# Fit a GARCH(1,1) model
spec_garch11 <- ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                          mean.model = list(armaOrder=c(0,0)))
fit_garch11 <- ugarchfit(spec=spec_garch11, data=r)
fit_garch11
plot(fit_garch11, which=11) 

# The mean model defaults to ARMA(1,1) if omitted. You can explore different mean equations.
spec_garch11_arma <- ugarchspec(variance.model = list(garchOrder=c(1,1)))
show(spec_garch11_arma)
fit_garch11_arma <- ugarchfit(spec=spec_garch11_arma, data=r,
                              solver.control = list(trace=1)) # examine the iteration process
fit_garch11_arma
plot(fit_garch11_arma)
infocriteria(fit_garch11_arma) # calculate information criteria


# Fit a GARCH(1,1) model with student-t distribution
spec_garch11std <- ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                           mean.model = list(armaOrder=c(0,0)),
                           distribution.model = "std")
fit_garch11std <- ugarchfit(spec=spec_garch11std, data=r)
fit_garch11std
plot(fit_garch11std)


# forecasting
(fcst_garch11 <- ugarchforecast(fit_garch11, n.ahead=10))
plot(fcst_garch11, which=1)
plot(fcst_garch11, which=3)

(fcst_garch11std <- ugarchforecast(fit_garch11std, n.ahead=10))
plot(fcst_garch11std, which=1)
plot(fcst_garch11std, which=3)


# {rugarch} reference manual
# https://cran.r-project.org/web/packages/rugarch/rugarch.pdf
# vignettes
# https://cran.r-project.org/web/packages/rugarch/vignettes/Introduction_to_the_rugarch_package.pdf

