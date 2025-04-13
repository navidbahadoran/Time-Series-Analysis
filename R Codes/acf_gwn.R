# Comments start with #

# ===== Simulate a Gaussian white noise =====
y <- rnorm(n=100, mean=0, sd=0.2)
?rnorm # check out the help manual
plot.ts(y)
abline(h=0, col="blue", lty="dashed") # add a horizontal line for the mean
y
head(y) # view the first part of y
head(y,10)
tail(y) # view the last part of y
# some basic operation
length(y)
T = 100
y[2:T] # the same as y[-1]
y[-1]
length(y[-1])
all.equal(y[-1],y[2:T]) # confirm they are the same


# === Compute (and by default plot) estimates of the autocorrelation function==== 
acf(y) 
# A chart of correlation statistics is called a correlogram.
# The horizontal blue lines are the approximate 95% CI's.
# The lines give the values beyond which the autocorrelations are 
# statistically significantly different from zero.
acf(y, lag=12)
acf(y, type = "covariance") # compute (and plot) autocovariance function
acf(y, plot=FALSE)
acf_y <- acf(y, plot=FALSE)
acf_y$acf
(acf(y)) # view both the plots and the values of ACF


# ===== Perform portmanteau test===== 
Box.test(y, lag=10, type='Ljung') # Ljung-Box statistic
Box.test(y, lag=10, type='Box') # Box-Pierce statistic

# Rule of thumb: Use lag=min(10,T/5) for non-seasonal data and 
# lag=min(2s, T/5) for seasonal data, where s is the period of seasonality 
# (Hyndman and Athanasopoulos, 2018)
