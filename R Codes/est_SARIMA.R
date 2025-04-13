# Example: M1 money supply
# Data sourced from Enders (2014). Also refer to pp. 98-101
# =========================================================
library(readxl)
library(forecast)
library(urca)
library(ggplot2)

#data <- read_excel("quarterly.xls")
data <- read_excel(file.choose())
M1_full <- ts(data$M1NSA, frequency=4, start=c(1960,1))  # 1960Q1 - 2012Q4
plot(M1_full, xlab="Year")
M1 <- window(M1_full, end=c(2011,4)) # exclude the last four observations


logM1 <- log(M1)
plot(logM1, xlab="Year")
ur_test_log <- ur.df(logM1, type="trend", lags=6, selectlags="AIC")
summary(ur_test_log)
plot(ur_test_log) # check residuals


mg <- 100*diff(logM1)
plot(mg, xlab="Year", main="M1 Growth")
tsdisplay(mg)


smg <- diff(mg, 4)
plot(smg, xlab="Year", main="Seasonally differenced M1 Growth")
tsdisplay(smg)

ur_test_smg <- ur.df(smg, type="drift", lags=6, selectlags="BIC")
summary(ur_test_smg)
plot(ur_test_smg)
 
# Estimate candidate models
(fit_AR1_SMA1 <- arima(smg, order=c(1,0,0), seasonal=c(0,0,1)))
checkresiduals(fit_AR1_SMA1)
checkresiduals(fit_AR1_SMA1, lag=12)

(fit_AR1_SAR1 <- arima(smg, order=c(1,0,0), seasonal=c(1,0,0)))
checkresiduals(fit_AR1_SAR1, lag=12)

(fit_MA1_SMA1 <- arima(smg, order=c(0,0,1), seasonal=c(0,0,1)))
checkresiduals(fit_MA1_SMA1, lag=12)

(fit_MA2_SMA1 <- arima(smg, order=c(0,0,2), seasonal=c(0,0,1)))
checkresiduals(fit_MA2_SMA1, lag=12)

(fit_AR2_SMA1 <- arima(smg, order=c(2,0,0), seasonal=c(0,0,1)))
checkresiduals(fit_AR2_SMA1, lag=12)

AIC(fit_AR1_SMA1, fit_AR1_SAR1, fit_MA1_SMA1, fit_MA2_SMA1, fit_AR2_SMA1)
BIC(fit_AR1_SMA1, fit_AR1_SAR1, fit_MA1_SMA1, fit_MA2_SMA1, fit_AR2_SMA1)


(fit_auto_smg <- auto.arima(smg)) # set stepwise=FALSE to do a full search over the model space
fcast_smg <- forecast(fit_auto_smg) # h defaults to 2*S=8 periods
plot(fcast_smg)

auto.arima(smg, ic="bic")

(fit_auto_mg <- auto.arima(mg))
fcast_mg <- forecast(fit_auto_mg)
plot(fcast_mg)

# The easiest approach to forecast the original M1 series is to 
# set lambda=0 within the modeling function.
(fit_select <- auto.arima(M1, ic="bic", d=1, lambda=0)) # set lambda=0 to apply log-transformation
(fcast_M1 <- forecast(fit_select))


autoplot(fcast_M1) + 
  autolayer(M1_full, color = 1, lwd = 0.8) +
  autolayer(fcast_M1$mean, color = 2, lwd = 0.8) +
  labs(y="M1 in billions", x="Year") +
  theme_bw()


