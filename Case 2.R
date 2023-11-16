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
  cpi = CPIAUCSL$CPIAUCSL,
  fed = FEDFUNDS$FEDFUNDS,
  ind = INDPRO$INDPRO
)

# Iterate over different lag orders
lag_orders <- 1:20  # Adjust the range based on your data and context

# Store AIC or BIC values for each lag order
aic_values <- numeric(length(lag_orders))
bic_values <- numeric(length(lag_orders))

# Run the VAR model over the different lag orders and store the IC's
for (i in seq_along(lag_orders)) {
  current_lag <- lag_orders[i]
  model <- VAR(data, p = current_lag, type = "const", lag.max = current_lag, season = 12)
  aic_values[i] <- AIC(model)
  bic_values[i] <- BIC(model)
}

# Choose the best IC
best_aic_order <- lag_orders[which.min(aic_values)]
best_bic_order <- lag_orders[which.min(bic_values)]






