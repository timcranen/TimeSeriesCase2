rm(list=ls())

library(tseries)
library(vars)

# Set working directory
setwd("~/Documents/Time series econometrics")

# Read in data from january 2010 untill december 2019 for respectively
# the federal funds rate, the cpi and the inductial production index

FEDFUNDS <- read.csv("FEDFUNDS.csv")
CPIAUCSL <- read.csv("CPIAUCSL.csv")
INDPRO <- read.csv("INDPRO.csv")

# Plotting the Federal Funds Rate
FEDFUNDS$DATE <- as.Date(FEDFUNDS$DATE)
plot(x = FEDFUNDS$DATE, y = FEDFUNDS$FEDFUNDS, type = "l", xlab="Date", ylab="Federal Funds Rate", main="Monthly Federal Funds Rate between 2010 and 2019")

# Plotting the CPI
CPIAUCSL$DATE <- as.Date(CPIAUCSL$DATE)
plot(x=CPIAUCSL$DATE, y=CPIAUCSL$CPIAUCSL, type="l", xlab="Date", ylab="CPI", main="Monthly CPI between 2010 and 2019")

# Plotting the industrial production
INDPRO$DATE <- as.Date(INDPRO$DATE)
plot(x=INDPRO$DATE, y=INDPRO$INDPRO, type="l", xlab="Date", ylab="Industrial Production", main="Monthly Industial Production between 2010 and 2019")

# No time series is stationary, so we have to transform the data


# Take difference data for the Federal Funds Rate
diffFEDFUNDS <- diff(FEDFUNDS$FEDFUNDS)
plot(x=diffFEDFUNDS, type="l")

# Augmented Dickey-Fuller test to test for stationarity
adf.test(diffFEDFUNDS)


# Take for the CPI
diffCPIAUCSL <- diff(diff(log(CPIAUCSL$CPIAUCSL)))
plot(x = diffCPIAUCSL, type="l")

# Augmented Dickey-Fuller test to test for stationarity
adf.test(diffCPIAUCSL)
# Take the log difference for the industrial production index
diffINDPRO <- diff(log(INDPRO$INDPRO))
plot(x = diffINDPRO, type="l")

# Augmented Dickey-Fuller test to test for stationarity
adf.test(diffINDPRO)


# Determining the lag order of the VAR(p) process
data <- data.frame(
  cpi = diffCPIAUCSL,
  fed = diffFEDFUNDS[1:598],
  ind = diffINDPRO[1:598]
)

VARselect(y = data, lag.max = 20, season = 12)



# GRANGER CAUSALITY

# Estimating the var model for 3 lags
var_model <- VAR(data, p = 3, season = 12, ic = "HQ")

# Create matrix
matrix <- as.matrix(data)

# Test for Granger Causality, we check the bivariate cases and see if they Granger cause
# single variable not in the bivariate cause matric given in the function. 

# Perform the Granger Causality Test (H0: fed and ind do not Granger-cause cpi)
granger_test_cpi <- causality(var_model, cause = c("fed", "ind"))
granger_test_cpi

# Perform the Granger Causality Test (H0: fed and cpi do not Granger-cause ind)
granger_test_ind <- causality(var_model, cause = c("fed", "cpi"))
granger_test_ind

# Perform the Granger Causality Test (H0: fed and cpi do not Granger-cause ind)
granger_test_fed <- causality(var_model, cause = c("ind", "cpi"))
granger_test_fed

