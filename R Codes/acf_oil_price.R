# install.packages("quantmod")
library(quantmod)

# Daily Crude Oil Prices: Brent
# https://fred.stlouisfed.org/series/DCOILBRENTEU
getSymbols("DCOILBRENTEU", src="FRED")   
# xts: eXtensible Time Series-- a matrix with associated time series 
# for each observation
# ?getSymbols    # check out the usage

plot(DCOILBRENTEU)
p <- DCOILBRENTEU['2010/2019']
head(p)
tail(p)
plot(p)
length(p)
nrow(p)

# remove missing values
p_reg <- p[!is.na(p)]
tail(p_reg)  # check that NA is removed

logp <- log(p_reg) # log oil price
plot(logp)

rt <- diff(logp)*100 # return of oil price (in percentages)
rt <- rt[-1]  # remove the first NA value, otherwise sd(), mean(), acf() won't work
plot(rt)
summary(rt)
sd(rt)
mean(rt)
hist(rt)  # draw histogram


(acf(p_reg))
(acf(logp))
(acf(rt))


Box.test(rt, lag=10) # defaults to Box-Pierce test
Box.test(rt, lag=10, type="Ljung-Box")

# Clear environment
# Please be careful as there is no warning
# rm(list=ls())
