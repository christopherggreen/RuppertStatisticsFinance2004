###########################################################
#                                                         #
# Copyright (c) Christopher G. Green, 2006                #
#                                                         #
###########################################################
############################################################
#                                                          #
# figure13-19.q                                            #
#                                                          #
############################################################

# run figure13-18.q first
fit.25.lambda.zerofive <- Pspline05(rate.eu01, diff.eu01, nknots=25, degree=2, igcv=F, penwt=0.5)
fit.25.lambda.fifty    <- Pspline05(rate.eu01, diff.eu01, nknots=25, degree=2, igcv=F, penwt=50)

# Ruppert's figure is mislabeled (swap the point five and the fifty)
par(lwd=3,las=1)
ind <- order(rate.eu01)
plot(  rate.eu01[ind], fit.25.lambda.five$yhat[ind], type="l", lty=1, ylim=c(-0.015,0.002))
lines( rate.eu01[ind], fit.25.lambda.zerofive$yhat[ind], type="l", lty=4)
lines( rate.eu01[ind], fit.25.lambda.fifty$yhat[ind] , type="l", lty=3)
key(corner=c(0,0), border=T, 
	lines=list(lty=c(4,1,3)), 
	text=list(lty=c("lambda = 0.5", "lambda = 5", "lambda = 50")))
