############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure9-5.q                                              #
#                                                          #
############################################################

basis01 <- bs(usstrips$time2mat, degree=3, df=4 , B=c(0,30))
basis05 <- bs(usstrips$time2mat, degree=3, df=8 , B=c(0,30))
basis10 <- bs(usstrips$time2mat, degree=3, df=13, B=c(0,30))
basis20 <- bs(usstrips$time2mat, degree=3, df=23, B=c(0,30))

# these get the job done but are not pretty!
# probably better ways to fit these models
nls.model.01 <- nls( usstrips$price ~ 100*exp(
		-( beta1 * basis01[,1] + beta2 * basis01[,2] + beta3 * basis01[,3] + beta4 * basis01[,4] ) 
	), start=list(beta1=.01,beta2=.01,beta3=.01,beta4=.01) )
summary(nls.model.01)

nls.model.05 <- nls( usstrips$price ~ 100*exp(
		-( beta1 * basis05[,1] + beta2 * basis05[,2] + beta3 * basis05[,3] + beta4 * basis05[,4] + 
			beta5 * basis05[,5] + beta6 * basis05[,6] + beta7 * basis05[,7] + beta8 * basis05[,8] ) 
	), start=list(beta1=.01,beta2=.01,beta3=.01,beta4=.01,beta5=.01,beta6=.01,beta7=.01,beta8=.01) )
summary(nls.model.05)

nls.model.10 <- nls( usstrips$price ~ 100*exp(
		-( beta1 * basis10[,1] + beta2 * basis10[,2] + beta3 * basis10[,3] + beta4 * basis10[,4] + 
			beta5 * basis10[,5] + beta6 * basis10[,6] + beta7 * basis10[,7] + beta8 * basis10[,8] + 
			beta9 * basis10[,9] + beta10 * basis10[,10] + beta11 * basis10[,11] + beta12 * basis10[,12] +
			beta13 * basis10[,13] ) 
	), start=list(beta1=.01,beta2=.01,beta3=.01,beta4=.01,beta5=.01,beta6=.01,beta7=.01,beta8=.01,
		beta9=.01,beta10=.01,beta11=.01,beta12=.01,beta13=.01) )
summary(nls.model.10)

nls.model.20 <- nls( usstrips$price ~ 100*exp(
		-( beta1 * basis20[,1] + beta2 * basis20[,2] + beta3 * basis20[,3] + beta4 * basis20[,4] + 
			beta5 * basis20[,5] + beta6 * basis20[,6] + beta7 * basis20[,7] + beta8 * basis20[,8] + 
			beta9 * basis20[,9] + beta10 * basis20[,10] + beta11 * basis20[,11] + beta12 * basis20[,12] + 
			beta13 * basis20[,13] + beta14 * basis20[,14] + beta15 * basis20[,15] + beta16 * basis20[,16] +
			beta17 * basis20[,17] + beta18 * basis20[,18] + beta19 * basis20[,19] + beta20 * basis20[,20] +
			beta21 * basis20[,21] + beta22 * basis20[,22] + beta23 * basis20[,23]
			) 
	), start=list(beta1=.01,beta2=.01,beta3=.01,beta4=.01,beta5=.01,beta6=.01,beta7=.01,beta8=.01,
		beta9=.01,beta10=.01,beta11=.01,beta12=.01,beta13=.01,beta14=.01,beta15=.01,beta16=.01,
		beta17=.01,beta18=.01,beta19=.01,beta20=.01,beta21=.01,beta22=.01,beta23=.01) )
summary(nls.model.20)

par(xaxs="i",yaxs="i",las=1,lwd=2)
plot(usstrips$time2mat, predict(basis01, usstrips$time2mat, derivs=1) %*% coef(nls.model.01), 
	type="l", xlab="time in years", ylab="forward rate")
lines(usstrips$time2mat, predict(basis10, usstrips$time2mat, derivs=1) %*% coef(nls.model.10), lty=4)
lines(usstrips$time2mat, predict(basis20, usstrips$time2mat, derivs=1) %*% coef(nls.model.20), lwd=2)
key(corner=c(0,0), border=T, text=list(c("1 knot","10 knots","20 knots")), 
	lines=list(lty=c(1,4,1),lwd=c(1,1,2)))
