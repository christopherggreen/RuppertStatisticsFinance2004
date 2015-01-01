############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page117.q                                                #
#                                                          #
############################################################

logR.fit.mle <- arima.mle(logR, model=list(order=c(6,0,0)), xreg=rep(1,length(logR)))
logR.fit.mle
arima.diag(logR.fit.mle, plot=F)

logR.fit.yw <- ar.yw(logR, order.max=6)
summary.ar(logR.fit.yw)
