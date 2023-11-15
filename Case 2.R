rm(list=ls())

# Set working directory
setwd("~/Documents/Time series econometrics")

# Read in data from january 2010 untill december 2019 for respectively
# the federal funds rate, the cpi and the inductial production index

dataFedFunds <- read.csv("FEDFUNDS.csv")
dataCPI <- read.csv("CPIAUCSL.csv")
dataInd <- read.csv("INDPRO.csv")

# Plotting the Federal Funds Rate
dataFedFunds$DATE <- as.Date(dataFedFunds$DATE)
plot(x = dataFedFunds$DATE, y = dataFedFunds$FEDFUNDS, type = "l", xlab="Date", ylab="Federal Funds Rate", main="Monthly Federal Funds Rate between 2010 and 2019")

# Plotting the CPI
dataCPI$DATE <- as.Date(dataCPI$DATE)
plot(x=dataCPI$DATE, y=dataCPI$CPIAUCSL, type="l", xlab="Date", ylab="CPI", main="Monthly CPI between 2010 and 2019")

# Plotting the industrial production
dataInd$DATE <- as.Date(dataInd$DATE)
plot(x=dataInd$DATE, y=dataInd$INDPRO, type="l", xlab="Date", ylab="Industrial Production", main="Monthly Industial Production between 2010 and 2019")

# No time series is stationary, so we have to transform the data
