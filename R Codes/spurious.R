# Spurious regression
# ===================
library(tseries)
T = 100
set.seed(300)
RW1 <- ts(cumsum(rnorm(T)))
RW2 <- ts(cumsum(rnorm(T)))
seqplot.ts(RW1, RW2)  # plot two time series in the same figure

# Spurious regression
model <- lm(RW1~RW2)
summary(model)
acf(resid(model))

# Regression using the differenced series
model_fd <- lm(diff(RW1)~diff(RW2))
summary(model_fd)
