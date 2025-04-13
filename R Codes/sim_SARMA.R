# Simulate SARMA processes
# ==========================
library(sarima)
library(forecast)

sma <- sim_sarima(n=1000, model=list(nseasons=4, sma=0.3)) # try nseasons=12
plot.ts(sma[1:100]) # zooming in for close inspection
tsdisplay(sma)

sar <- sim_sarima(n=1000, model=list(nseasons=4, sar=0.7))
plot.ts(sar[1:100])
tsdisplay(sar)

y1 <- sim_sarima(n=1000, model=list(nseasons=12, ma=0.7, sma=0.3)) # try sma=-0.3
plot.ts(y1[1:100])
tsdisplay(y1, lag.max=40)

y2 <- sim_sarima(n=1000, model=list(nseasons=12, ar=0.7, sar=0.3))
plot.ts(y2[1:100])
tsdisplay(y2, lag.max=40)
