# Simulate the DF distribution without a constant or trend
# ==========================================================
T <- 2000
reps <- 5000

DF_nc <- rep(NA, reps)

for (i in 1:reps){
  u <- rnorm(T)
  W <- 1/sqrt(T)*cumsum(u)
  DF_nc[i] <- (W[T]^2-1)/(2*sqrt(mean(W^2)))
}

x <- seq(-4, 4, by=.1)
plot(density(DF_nc), lwd=2, col="red")
lines(x, dnorm(x), lwd=2, col="blue", lty="dashed")
legend(x= "topright",legend=c("DF distribution (no constant or trend)","Standard normal distribution"),
       col=c("red", "blue"), lty=1:2)


# Compare the quantiles of standard normal and DF distributions
library(fUnitRoots)
qnorm(c(0.01,0.05,0.10))
qunitroot(c(0.01,0.05,0.10), trend="nc") # sample size defaults to Inf
qunitroot(c(0.01,0.05,0.10), trend="c") 
qunitroot(c(0.01,0.05,0.10), trend="ct")
    # Including a constant and trend shifts the DF distributions to the left
adfTable(trend="c") # tables critical values for the DF test
    # trend = c("nc", "c", "ct")

