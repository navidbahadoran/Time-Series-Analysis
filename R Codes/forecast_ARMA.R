# Forecasting with ARMA models
# ============================
library(forecast)
library(ggplot2)
set.seed(125)
y <- arima.sim(n=120, list(ar=0.7)) + 3
y <- ts(y, frequency=12, start=c(2010,1))
plot(y)
ar1 <- arima(y, order=c(1,0,0))
ar1
checkresiduals(ar1)

# ? forecast  # check out the help documentation for details
ycast <- forecast(ar1, h=12)  # out-of-sample forecasting
# ycast <- forecast(ar1, h=10, fan = TRUE) # set fan=TRUE for fan charts
ycast
plot(ycast)
abline(h=3, col="blue", lty="dashed")

autoplot(ycast)

# Alternatively, one may use the generic function predict()
ypred <- predict(ar1, 12) 
plot(ypred$pred)
ypred$pred
ypred$se


# ---In-sample forecasting and validation---
y_training <-window(y, end = c(2018,12))  # training data, or in-sample data
y_test <-window(y, start = c(2019,1))  # test data, or out-of-sample data


# Fit three candidate models to the training data
ar1 <- arima(y_training, order=c(1,0,0))
autoplot(ar1)
checkresiduals(ar1)
ma1 <- arima(y_training, order=c(0,0,1)) 
autoplot(ma1)
checkresiduals(ma1)
arma11 <- arima(y_training, order=c(1,0,1))
autoplot(arma11)
checkresiduals(arma11)
# try out auto.arima()
auto <-auto.arima(y_training)
auto

# Forecasting using the AR(1) model
fcast_ar1 <- forecast(ar1, h = 12)
autoplot(fcast_ar1) +
  autolayer(y, color = 1, lwd = 0.8) +
  autolayer(fcast_ar1$mean, series = "Forecasts", lwd = 0.8) + 
  theme_bw()
accuracy(fcast_ar1, y_test)

# Forecasting using the MA(1) model
fcast_ma1 <- forecast(ma1, h = 12)
autoplot(fcast_ma1) +
  autolayer(y, color = 1, lwd = 0.8) +
  autolayer(fcast_ma1$mean, series = "Forecasts", lwd = 0.8) +
  theme_bw()
accuracy(fcast_ma1, y_test)

# Forecasting using the ARMA(1,1) model
fcast_arma11 <- forecast(arma11, h = 12)
autoplot(fcast_arma11) +
  autolayer(y, color = 1, lwd = 0.8) +
  autolayer(fcast_arma11$mean, series = "Forecasts", lwd = 0.8) +
  theme_bw()
accuracy(fcast_arma11, y_test)

# Forecasting using the best model selected by auto.arima()
fcast_auto <- forecast(auto, h = 12)
autoplot(fcast_auto) +
  autolayer(y, color = 1, lwd = 0.8) +
  autolayer(fcast_auto$mean, series = "Forecasts", lwd = 0.8) +
  theme_bw()
accuracy(fcast_auto, y_test)

# Compare forecasts by plotting them together
autoplot(y, lwd = 0.5) +
  autolayer(fcast_ar1, series="AR(1)", PI=FALSE, size = 0.8) +
  autolayer(fcast_ma1, series="MA(1)", PI=FALSE, size = 0.8) +
  autolayer(fcast_arma11, series="ARMA(1,1)", PI=FALSE, size = 0.8) +
  theme_bw()

