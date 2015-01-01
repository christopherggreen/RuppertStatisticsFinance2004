############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure9-4.q                                              #
#                                                          #
############################################################

# run page319-320.q first
# need the constant model

nls.model.constant <- nls( price ~ 100*exp(-beta0*time2mat), 
	data=usstrips, start=list(beta0=.01) )	
# fit piecewise quadratic function with one knot at T=15

time2mat.lower <- time2mat.upper <- usstrips$time2mat
time2mat.upper <- time2mat.upper - 15
time2mat.upper[time2mat.upper < 0] <- 0
	
nls.model.pwquad <- nls( usstrips$price ~ 
	100*exp(-( 
		beta0*time2mat.lower + (beta1*time2mat.lower^2)/2 + (beta2*time2mat.lower^3)/3 + (beta6*time2mat.upper^3)/3 
		)), start=list(beta0=.01,beta1=-.005,beta2=-.005,beta6=-.005),
		control=nls.control(maxiter=100))
		
summary(nls.model.pwquad)
#plot(usstrips$time2mat, 
#	 cbind(1,time2mat.lower,time2mat.lower^2,time2mat.upper^2) %*% coef(nls.model.pwquad))
	
par(lwd=2,xaxs="i",yaxs="i",las=1)
plot( usstrips$time2mat, coef(nls.model.constant)*rep(1,length=nrow(usstrips)), 
	type="l", 	xlab="time in years", ylab="forward rate", lwd=3, ylim=c(0.04,0.08) )
lines( usstrips$time2mat, cbind(1,usstrips$time2mat) %*% coef(nls.model.linear), lty=3)	
lines( usstrips$time2mat, 
	cbind(1,usstrips$time2mat,usstrips$time2mat^2) %*% coef(nls.model.quadratic), lty=4)
lines( usstrips$time2mat, 
	cbind(1,time2mat.lower,time2mat.lower^2,time2mat.upper^2) %*% coef(nls.model.pwquad), lty=1)		 	
key(corner=c(0,1), border=T, text=list(c("constant","linear","quadratic","cubic")),
	lines=list(lty=c(1,3,4,1),lwd=c(3,2,2,2)))
