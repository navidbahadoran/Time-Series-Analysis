# ECO 5428, Spring 2025
# Cointegration and VECM
# ======================
library(urca)
library(vars)
library(forecast)
library(tsDyn)

# load the data set for Finland, used by Johansen and Juseliues (1990)
# https://onlinelibrary.wiley.com/doi/10.1111/j.1468-0084.1990.mp52002003.x
# This example is for illustration only — not intended for replication.

data(finland) # included in {urca}
? finland   
# Contains 4 variables over 1958q1-1984q3

# data(denmark) # You can also explore the dataset for Denmark used in the same study.

y<- ts(finland, frequency=4, start=c(1958,1))
plot(y, xlab="Year")

# Create centered quarterly dummy variables, only used in the VECM() function
df_q <- expand.grid(1:4, 1958:1984)[1:nrow(y),]
colnames(df_q) <- c("q", "yr")
df_q$Q1 <- (df_q$q==1)-1/4
df_q$Q2 <- (df_q$q==2)-1/4
df_q$Q3 <- (df_q$q==3)-1/4
dum_seas <- df_q[,-c(1,2)]


lagselect <- VARselect(y, lag.max = 4, type = "const", season=4)
lagselect$selection

# Johansen's trace test
# K: the lag order of the series (levels) in the VAR
# spec is set s.t. the Gamma matrices measure transitory effects (see help manual)
# ecdet is set to "none" following Johansen and Juseliues (1990)
ctest_tr <- ca.jo(y, type  = "trace", ecdet = "none", K = 2, 
                  spec = "transitory", season = 4)
summary(ctest_tr)

# Johansen's maximum eigenvalue test
ctest_eig <- ca.jo(y, type  = "eigen", ecdet = "none", K = 2, 
                   spec = "transitory", season = 4)
summary(ctest_eig)


# VECM estimation (using the VECM function from {tsDyn})
# lag: Number of lags in the VECM representation (not in the VAR)
# r: Number of cointegrating relations
# Johansen and Juseliues (1990) actually chose 3 cointegrating relations.
VECM_model <- VECM(y, lag=1, r=2, estim = "ML", LRinclude = "none", exogen = dum_seas)
summary(VECM_model)

# ?rank.test  # Also check out this command from {tsDyn}

# Alternative method: OLS regression of restricted VECM (using the cajorls function from {urca})
cajorls_model <- cajorls(ctest_tr, r=2)  
summary(cajorls_model$rlm)

# Transform VECM to VAR in levels (using the vec2var function from {vars})
VAR_model <- vec2var(ctest_tr, r=2)
VAR_model

# Diagnostic checks as before (serial dependence, ARCH effects, normality, etc.)
serial_test <- serial.test(VAR_model, lags.pt = 5, type = "PT.adjusted")
serial_test
# plot(serial_test)

# If the model is adequate, can proceed with impulse response analysis, FEVD, 
# and forecasting using the VAR model

# I also recommend reading the Stata help documentation for VECMs.
# https://www.stata.com/manuals13/tsvecintro.pdf
