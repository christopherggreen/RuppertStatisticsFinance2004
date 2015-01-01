###########################################################
#                                                         #
# Copyright (c) Christopher G. Green, 2006                #
#                                                         #
###########################################################
############################################################
#                                                          #
# figure13-2.q                                             #
#                                                          #
############################################################

# run figure13-1.q first
# needs Pspline05
#
diff.eu01 <- diff(eurodata$eu01)
rate.eu01 <- eurodata$eu01[-nrow(eurodata)]
ind <- order(rate.eu01)

par(mfrow=c(2,1), las=1)

plot( rate.eu01, diff.eu01, type="p", pch=".", xlab="Euro rate", ylab="drift")
fit <- Pspline05(rate.eu01, diff.eu01, nknots=20, degree=2)
lines( rate.eu01[ind], fit$yhat[ind], lwd=2)

plot( rate.eu01, diff.eu01^2, type="p", pch=".", xlab="Euro rate", ylab="volatility^2")
fit <- Pspline05(rate.eu01, diff.eu01^2, nknots=20, degree=2)
lines( rate.eu01[ind], fit$yhat[ind], lwd=2)
	
par(mfrow=c(1,1))
