rm(list=ls())

library(tseries)
library(vars)
library(MASS)
library(car)

# Set working directory
setwd("~/Documents/Time series econometrics")

# Read in data from january 2010 untill december 2019 for respectively
# the federal funds rate, the cpi and the inductial production index

data_set <- read.csv("2020-01.csv")
CPIAUCSL <- data_set$CPIAUCSL[2:733]             #CPI
INDPRO <- data_set$INDPRO[2:733]                 #Industrial Production Index
FEDFUNDS <- data_set$FEDFUNDS[2:733]             #Federal Funds Rate
DATE <- data_set$sasdate[2:733]
DATE <- as.Date(DATE, format="%m/%d/%Y")

# Plotting the Federal Funds Rate
plot(x = DATE, y = FEDFUNDS, type = "l", xlab="Date", ylab="Federal Funds Rate", main="Monthly Federal Funds Rate between 2010 and 2019")

# Plotting the CPI
plot(x=DATE, y=CPIAUCSL, type="l", xlab="Date", ylab="CPI", main="Monthly CPI between 2010 and 2019")

# Plotting the industrial production
plot(x=DATE, y=INDPRO, type="l", xlab="Date", ylab="Industrial Production", main="Monthly Industial Production between 2010 and 2019")



# TRANSFORMING THE DATA (according to the codes given by McCracken & Ng(2016))

# Take difference data for the Federal Funds Rate
diffFEDFUNDS <- diff(FEDFUNDS)
plot(diffFEDFUNDS, type="l")

# Augmented Dickey-Fuller test to test for stationarity
adf.test(diffFEDFUNDS)


# Take difference of difference of log for the CPI
diffCPIAUCSL <- diff(diff(log(CPIAUCSL)))
plot(diffCPIAUCSL, type="l")

# Augmented Dickey-Fuller test to test for stationarity
adf.test(diffCPIAUCSL)


# Take the log difference for the industrial production index
diffINDPRO <- diff(log(INDPRO))
plot(diffINDPRO, type="l")

# Augmented Dickey-Fuller test to test for stationarity
adf.test(diffINDPRO)


# Determining the lag order of the VAR(p) process
data <- data.frame(
  cpi = diffCPIAUCSL,
  fed = diffFEDFUNDS[1:730],
  ind = diffINDPRO[1:730]
)

# VARselect command is a built-in R function to select the best number of lags associated with a certain 
# information criterion
VARselect(y = data, lag.max = 24, type="const")

# Estimating the var model for 3 lags
var_model <- VAR(data, p = 3, ic = "SC")

#Check Validity of the VAR Model

# QQplots to check normality in the residuals:

qqnorm(y=var_model$varresult$cpi$residuals)
qqline(y=var_model$varresult$cpi$residuals, col=2)

qqnorm(y=var_model$varresult$fed$residuals)
qqline(y=var_model$varresult$fed$residuals, col=2)

qqnorm(y=var_model$varresult$ind$residuals)
qqline(y=var_model$varresult$ind$residuals, col=2)

# We see that the tails are slightly off the qqline, so the tails may not be entirely follow a normal distribution,
# but in the middle of the plot the dots follow the qqline, indicating normal distributed residuals in the 
# middle of the distribution

# Breusch-Pagan heteroskedasticity test, null hypothesis: homoskedastistic standard errors.
bptest(var_model$varresult$cpi)   #bp test on cpi

bptest(var_model$varresult$fed)   #bp test on fed

bptest(var_model$varresult$ind)   #bp test on ind

# As outliers can contribute to Heteroskedasticity and we saw in the QQplot that the tails do no follow a
# normal distribution, this can be the cause of our heteroskedastistic standard errors

#ACF
acf(var_model$varresult$cpi$residuals)   #acf of cpi
acf(var_model$varresult$fed$residuals)   #acf of fed
acf(var_model$varresult$ind$residuals)   #acf of ind

# No significant autocorrelation (in the slides the significance dotted line is at 0.2). So the model does
# not show signs of autocorrelation, meaning the lag order is well-defined. 

# No significant autocorrelation in the residuals

# GRANGER CAUSALITY

# Estimating the var model for 3 lags
var_model <- VAR(data, p = 3, ic = "SC")

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

# Create vectors for 1lag, 2lag and 3 lags to use in a regression, also the vectors to regress on are created
# (of the transformed data)
cpi_3lag <- data$cpi[1:727]
cpi_2lag <- data$cpi[2:728]
cpi_1lag <- data$cpi[3:729]
cpi_granger <- data$cpi[4:730]

fed_3lag <- data$fed[1:727]
fed_2lag <- data$fed[2:728]
fed_1lag <- data$fed[3:729]
fed_granger <- data$fed[4:730]

ind_3lag <- data$ind[1:727]
ind_2lag <- data$ind[2:728]
ind_1lag <- data$ind[3:729]
ind_granger <- data$ind[4:730]

# OLS, if one lag of a variable is significantly different from zero, the variable Granger causes
# the variable it is regressed on. 
biv_granger_cpi <- lm(cpi_granger ~ cpi_1lag+cpi_2lag+cpi_3lag+fed_1lag+fed_2lag+fed_3lag+ind_1lag+ind_2lag+ind_3lag)
summary(biv_granger_cpi)

# Does fed Granger cause cpi?
linearHypothesis(biv_granger_cpi, hypothesis.matrix = c("fed_1lag","fed_2lag", "fed_3lag"))

# Does ind Granger cause cpi?
linearHypothesis(biv_granger_cpi, hypothesis.matrix = c("ind_1lag","ind_2lag", "ind_3lag"))


# OLS for the federal funds rate to test for Granger causality. 
biv_granger_fed <- lm(fed_granger ~ cpi_1lag+cpi_2lag+cpi_3lag+fed_1lag+fed_2lag+fed_3lag+ind_1lag+ind_2lag+ind_3lag)
summary(biv_granger_fed)

# Does cpi Granger cause fed?
linearHypothesis(biv_granger_fed, hypothesis.matrix = c("cpi_1lag","cpi_2lag", "cpi_3lag"))

#Does ind Granger cause fed?
linearHypothesis(biv_granger_fed, hypothesis.matrix = c("ind_1lag","ind_2lag", "ind_3lag"))



# OLS for the industrial production index to test for Granger causality
biv_granger_ind <- lm(ind_granger ~ cpi_1lag+cpi_2lag+cpi_3lag+fed_1lag+fed_2lag+fed_3lag+ind_1lag+ind_2lag+ind_3lag)
summary(biv_granger_ind)

# Does cpi Granger cause ind?
linearHypothesis(biv_granger_ind, hypothesis.matrix = c("cpi_1lag","cpi_2lag", "cpi_3lag"))

# Does fed Granger cause ind?
linearHypothesis(biv_granger_ind, hypothesis.matrix = c("fed_1lag","fed_2lag", "fed_3lag"))



# IMPULSE RESPONSE FUNCTIONS FOR REDUCED-FORM VARS

plot(irf(var_model, impulse= "ind", response = "fed", n.ahead = 10), main = 'IRF plot ind on fed')

plot(irf(var_model, impulse= "ind", response = "cpi", n.ahead = 10), main = 'IRF plot ind on cpi')

plot(irf(var_model, impulse= "fed", response = "ind", n.ahead = 10), main = 'IRF plot fed on ind')

plot(irf(var_model, impulse= "fed", response = "cpi", n.ahead = 10), main = 'IRF plot fed on cpi')

plot(irf(var_model, impulse= "cpi", response = "fed", n.ahead = 10), main = 'IRF plot cpi on fed')

plot(irf(var_model, impulse= "cpi", response = "ind", n.ahead = 10), main = 'IRF plot cpi on ind')






# REDUCED FORM VARS VS STRUCTURAL VARS

amat <- diag(3)
amat[3,2] <- NA
amat[3,1] <- NA
amat[2,1] <- NA
amat

#restrictions don't allow fed at t to affect cpi or ind at t, or cpi at t to affect pro at t
#Need to work on alternatives?

#Making the svar:

SVAR1 <- SVAR(var_model1, Amat=amat, Bmat=NULL,hessian=TRUE, estmethod=c("scoring","direct"))
SVAR1

#SVAR impulse response functions:

#Shock on Production
SVARpop <- irf(SVAR1, impulse="ind", response="ind", ci = 0.95, boot=TRUE, runs=499)
SVARpop
plot1 <- plot(SVARpop)

SVARpof <- irf(SVAR1, impulse="ind", response="fed", ci = 0.95, boot=TRUE, runs=499)
SVARpof
plot2 <- plot(SVARpof)

SVARpoc <- irf(SVAR1, impulse="ind", response="cpi", ci = 0.95, boot=TRUE, runs=499)
SVARpoc
plot3 <- plot(SVARpoc)

#Shock on fed
SVARfop <- irf(SVAR1, impulse="fed", response="ind", ci = 0.95, boot=TRUE, runs=499)
SVARfop
plot(SVARfop)

SVARfof <- irf(SVAR1, impulse="fed", response="fed", ci = 0.95, boot=TRUE, runs=499)
SVARfof
plot(SVARfof)

SVARfoc <- irf(SVAR1, impulse="fed", response="cpi", ci = 0.95, boot=TRUE, runs=499)
SVARfoc
plot(SVARfoc)

#Shock on cpi
SVARcop <- irf(SVAR1, impulse="cpi",response="ind", ci = 0.95, boot=TRUE, runs=499)
SVARcop
plot(SVARcop)

SVARcof <- irf(SVAR1, impulse="cpi", response="fed", ci = 0.95, boot=TRUE, runs=499)
SVARcof
plot(SVARcof)

SVARcoc <- irf(SVAR1, impulse="cpi", response="cpi", ci = 0.95, boot=TRUE, runs=499)
SVARcoc
plot(SVARcoc)


#######Testing for different orderings#########

#Making a series of orderings

#CPI-FED-IND
data_2 <- data.frame(
  
  cpi = diffCPIAUCSL,
  fed = diffFEDFUNDS[1:730],
  ind = diffINDPRO[1:730]
)
#CPI-IND-FED
data_3 <- data.frame(
  cpi = diffCPIAUCSL,
  ind = diffINDPRO[1:730],
  fed = diffFEDFUNDS[1:730]
)
#FED-CPi-IND
data_4 <- data.frame(
  fed = diffFEDFUNDS[1:730],
  cpi = diffCPIAUCSL,
  ind = diffINDPRO[1:730]
)
#FED-IND-CPI
data_5 <- data.frame(
  fed = diffFEDFUNDS[1:730],
  ind = diffINDPRO[1:730],
  cpi = diffCPIAUCSL
)
#IND-FED-CPI
data_6 <- data.frame( 
  ind = diffINDPRO[1:730],
  fed = diffFEDFUNDS[1:730],
  cpi = diffCPIAUCSL
)

#Making the VARs:
var_model_2 <- VAR(data_2, p = 3, season = 12, ic = "SC")
var_model_3 <- VAR(data_3, p = 3, season = 12, ic = "SC")
var_model_4 <- VAR(data_4, p = 3, season = 12, ic = "SC")
var_model_5 <- VAR(data_5, p = 3, season = 12, ic = "SC")
var_model_6 <- VAR(data_6, p = 3, season = 12, ic = "SC")

#Making the recursive SVARs:


amat <- diag(3)
amat[3,2] <- NA
amat[3,1] <- NA
amat[2,1] <- NA
amat

SVAR2 <- SVAR(var_model_2, Amat=amat, Bmat=NULL,hessian=TRUE, estmethod=c("scoring","direct"))
SVAR3 <- SVAR(var_model_3, Amat=amat, Bmat=NULL,hessian=TRUE, estmethod=c("scoring","direct"))
SVAR4 <- SVAR(var_model_4, Amat=amat, Bmat=NULL,hessian=TRUE, estmethod=c("scoring","direct"))
SVAR5 <- SVAR(var_model_5, Amat=amat, Bmat=NULL,hessian=TRUE, estmethod=c("scoring","direct"))
SVAR6 <- SVAR(var_model_6, Amat=amat, Bmat=NULL,hessian=TRUE, estmethod=c("scoring","direct"))


irf.svarcpi_2 <- irf(var_model_2, response="cpi", ci = 0.95, boot=TRUE, runs=199)
irf.svarfed_2 <- irf(var_model_2, response="fed", ci = 0.95, boot=TRUE, runs=199)
irf.svarind_2 <- irf(var_model_2, response ="ind", ci = 0.95, boot=TRUE, runs=199)
plot(irf.svarcpi_2)
plot(irf.svarfed_2)
plot(irf.svarind_2)

irf.svarcpi_3 <- irf(var_model_3, response ="cpi", ci = 0.95, boot=TRUE, runs=199)
irf.svarfed_3 <- irf(var_model_3, response ="fed", ci = 0.95, boot=TRUE, runs=199)
irf.svarind_3 <- irf(var_model_3, response ="ind", ci = 0.95, boot=TRUE, runs=199)
plot(irf.svarcpi_3)
plot(irf.svarfed_3)
plot(irf.svarind_3)

irf.svarcpi_4 <- irf(var_model_4, response ="cpi", ci = 0.95, boot=TRUE, runs=199)
irf.svarfed_4 <- irf(var_model_4, response ="fed", ci = 0.95, boot=TRUE, runs=199)
irf.svarind_4 <- irf(var_model_4, response ="ind", ci = 0.95, boot=TRUE, runs=199)
plot(irf.svarcpi_4)
plot(irf.svarfed_4)
plot(irf.svarind_4)

irf.svarcpi_5 <- irf(var_model_5, response ="cpi", ci = 0.95, boot=TRUE, runs=199)
irf.svarfed_5 <- irf(var_model_5, response ="fed", ci = 0.95, boot=TRUE, runs=199)
irf.svarind_5 <- irf(var_model_5, response ="ind", ci = 0.95, boot=TRUE, runs=199)
plot(irf.svarcpi_5)
plot(irf.svarfed_5)
plot(irf.svarind_5)

irf.svarcpi_6 <- irf(var_model_6, response ="cpi", ci = 0.95, boot=TRUE, runs=199)
irf.svarfed_6 <- irf(var_model_6, response ="fed", ci = 0.95, boot=TRUE, runs=199)
irf.svarind_6 <- irf(var_model_6, response ="ind", ci = 0.95, boot=TRUE, runs=199)
plot(irf.svarcpi_6)
plot(irf.svarfed_6)
plot(irf.svarind_6)
