# ====simulating an AR(1) process====
set.seed(123)
ar1 <- arima.sim(n=100, list(ar=0.7))
    # try different values of the coefficient
plot.ts(ar1)
(acf(ar1))

# ====simulating an AR(2) process====
ar2 <-arima.sim(n=100, list(ar=c(0.6,0.2)))
plot.ts(ar2)
(acf(ar2))


ar2 <-arima.sim(n=10000, list(ar=c(0.5,-0.8)))
plot.ts(ar2)
(acf(ar2))

acos(0.28)  # arc-cosine
