############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-17.q                                            #
#                                                          #
############################################################

fit.02 <- Pspline05(rate.eu01, diff.eu01, nknots=2, degree=2)
fit.10 <- Pspline05(rate.eu01, diff.eu01, nknots=10, degree=2)
fit.20 <- Pspline05(rate.eu01, diff.eu01, nknots=20, degree=2)

par(lwd=3,las=1)
plot( rate.eu01, diff.eu01, type="p", pch=".", xlab="Euro rate",
	ylab="drift")
ind <- order(rate.eu01)
lines( rate.eu01[ind], fit.02$yhat[ind], lty=4)
lines( rate.eu01[ind], fit.10$yhat[ind], lty=1)
lines( rate.eu01[ind], fit.20$yhat[ind], lty=3)
key(0.03, -0.025, corner=c(0,0), border=T, 
	lines=list(lty=c(4,1,3)), 
	text=list(lty=c("2 knots", "10 knots", "20 knots")))
