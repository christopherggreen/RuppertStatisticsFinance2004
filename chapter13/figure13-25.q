############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-25.q                                            #
#                                                          #
############################################################

fit.vol <- Pspline05(rate.eu01, (diff.eu01-fit$yhat)^2, 
	nknots=15, degree=3)

par(mfrow=c(2,1), lwd=3)
ind <- order(rate.eu01)
plot( rate.eu01[ind], fit.vol$yhat[ind], type="l", 
	ylim=range(c(fit.vol$yhat,fit.vol$ulimit,fit.vol$llimit)),
	xlab="rate", ylab="drift")
lines( rate.eu01[ind], fit.vol$ulimit[ind], lty=3)
lines( rate.eu01[ind], fit.vol$llimit[ind], lty=3)
abline(h=0)
key(0.1,0.00015,corner=c(0,0),border=T, lines=list(lty=c(1,3)),
	text=list(c("Est.","95% CI")))
	
plot( rate.eu01[ind], fit.vol$yhatder[ind], type="l", 
	ylim=range(c(fit.vol$yhatder,fit.vol$ulimitder,fit.vol$llimitder)),
	xlab="rate", ylab="derivative of drift")
lines( rate.eu01[ind], fit.vol$ulimitder[ind], lty=3)
lines( rate.eu01[ind], fit.vol$llimitder[ind], lty=3)
abline(h=0)
key(0.1,0.002,corner=c(0,0),border=T, lines=list(lty=c(1,3)),
	text=list(c("Est.","95% CI")))	
	
par(mfrow=c(1,1))
