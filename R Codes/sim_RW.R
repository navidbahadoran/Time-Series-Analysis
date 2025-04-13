# Simulate random walks
# =======================

RW <- function(T, y0, mu, sig2) {
#  set.seed(100)
  t<-1:T
  eps <- cumsum(rnorm(n=T, mean=0, sd=sqrt(sig2))) # random steps are i.i.d. normal
  y <- y0 + t*mu + eps  # mu is the drift parameter
  return(y)
}

y1 <- RW(1000, 5, 0, 0.4)
plot(y1, type="l", xlab="t", main="Random Walk without Drift")
(acf(y1))


y2 <- RW(1000, 5, 0.1, 0.4)   # try a negative mu yourself
plot(y2, type="l", xlab="t", main="Random Walk with Drift")
(acf(y2))


# Another way to simulate random walks
y3 <- arima.sim(n=1000, model=list(order=c(0,1,0)), sd=sqrt(0.4))
plot(y3, type="l", xlab="t", main="Random Walk without Drift")

y4 <- 1:1001 * 0.1 + y3  # 0.1 is the drift parameter
plot(y4, type="l", xlab="t", main="Random Walk with Drift")
