rm(list=ls())
library(tseries)

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
plot(diffCPIAUCSL, type="l")

# Augmented Dickey-Fuller test to test for stationarity
adf.test(diffCPIAUCSL)

# Take the log difference for the industrial production index
diffINDPRO <- diff(log(INDPRO$INDPRO))
plot(diffINDPRO, type="l")

# Augmented Dickey-Fuller test to test for stationarity
adf.test(diffINDPRO)





