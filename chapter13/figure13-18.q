############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-18.q                                            #
#                                                          #
############################################################

fit.25.lambda.zero <- Pspline05(rate.eu01, diff.eu01, nknots=25, degree=2, igcv=F, penwt=0)
fit.25.lambda.five <- Pspline05(rate.eu01, diff.eu01, nknots=25, degree=2, igcv=F, penwt=5)
fit.25.lambda.inf  <- Pspline05(rate.eu01, diff.eu01, nknots=25, degree=2, igcv=F, penwt=1e10)

# Ruppert's figure is mislabeled, infinity is the quadratic, zero is the overfit
par(lwd=3,las=1)
ind <- order(rate.eu01)
plot(  rate.eu01[ind], fit.25.lambda.five$yhat[ind], type="l", lty=1, ylim=c(-0.015,0.002))
lines( rate.eu01[ind], fit.25.lambda.zero$yhat[ind], type="l", lty=4)
lines( rate.eu01[ind], fit.25.lambda.inf$yhat[ind] , type="l", lty=3)
key(corner=c(0,0), border=T, 
	lines=list(lty=c(4,1,3)), 
	text=list(lty=c("lambda = 0", "lambda = 5", "lambda = Inf")))
