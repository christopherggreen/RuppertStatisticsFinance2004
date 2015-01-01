############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-24.q                                            #
#                                                          #
############################################################

fit <- Pspline05(rate.eu01, diff.eu01, nknots=15, degree=3)

par(mfrow=c(2,1), lwd=3)
ind <- order(rate.eu01)
plot( rate.eu01[ind], fit$yhat[ind], type="l", 
	ylim=range(c(fit$yhat,fit$ulimit,fit$llimit)),
	xlab="rate", ylab="drift")
lines( rate.eu01[ind], fit$ulimit[ind], lty=3)
lines( rate.eu01[ind], fit$llimit[ind], lty=3)
abline(h=0)
key(0.1,-0.015,corner=c(0,0),border=T, lines=list(lty=c(1,3)),
	text=list(c("Est.","95% CI")))
	
plot( rate.eu01[ind], fit$yhatder[ind], type="l", 
	ylim=range(c(fit$yhatder,fit$ulimitder,fit$llimitder)),
	xlab="rate", ylab="derivative of drift")
lines( rate.eu01[ind], fit$ulimitder[ind], lty=3)
lines( rate.eu01[ind], fit$llimitder[ind], lty=3)
abline(h=0)
key(0.1,-0.95,corner=c(0,0),border=T, lines=list(lty=c(1,3)),
	text=list(c("Est.","95% CI")))	
	
par(mfrow=c(1,1))
