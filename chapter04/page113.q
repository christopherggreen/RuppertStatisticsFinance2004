############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page113.q                                                #
#                                                          #
############################################################

close <- read.table("../common/data/ge.dat",skip=1)[[1]]
logP <- log(close)
logR <- diff(logP)

# ols estimates
logR.fit.ols <- lm(logR[-1] ~ I(-logR[-length(logR)]))
summary(logR.fit.ols)

# yule-walker estimates 
#
logR.fit.yw <- ar.yw(logR, order.max=1)
# print out statistics
# need to load summary.ar.q first
summary.ar(logR.fit.yw)
acf.plot(logR.fit.yw)

# fit ar(1) model with explicit intercept
logR.fit.mle <- arima.mle(logR, model=list(order=c(1,0,0)), xreg=rep(1,length(logR)))
logR.fit.mle
# coefficients of the model
logR.fit.mle$model
# intercept
logR.fit.mle$reg.coef

arima.diag(logR.fit.mle, plot=F)

# figure 4.5
arima.diag(logR.fit.mle, plot=T)

