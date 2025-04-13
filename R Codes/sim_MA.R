# ====simulating an MA(1) process====
# set.seed(435)  # fix seed if you want to reproduce the random result
ma1 <- arima.sim(n=100, list(ma=0.7))
    # try different values of the coefficient
plot(ma1) # plot() understands that we have a time series object
abline(h=0, col="blue", lty="dashed")
(acf(ma1))

# let us make mean=5; simulation defaults to mean=0
ma1_mean5 = ma1 + 5  
plot(ma1_mean5)
abline(h=5, col="blue", lty="dashed")
(acf(ma1_mean5))

# try changing the standard deviation
ma1_sd <- arima.sim(n=100, list(ma=0.7), sd=0.2) 
plot(ma1_sd)
abline(h=0, col="blue", lty="dashed")
(acf(ma1_sd))

# ====simulating an MA(2) process====
ma2 <- arima.sim(n=100, list(ma=c(0.6,0.4)))
plot(ma2)
abline(h=0, col="blue", lty="dashed")
(acf(ma2))
