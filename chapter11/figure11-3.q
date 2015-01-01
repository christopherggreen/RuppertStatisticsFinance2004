############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure11-3.q                                             #
#                                                          #
############################################################

# diagnostic plots
nn <- 1000
par(mfrow=c(5,5),las=1)
for ( m in seq(1,nn,length=25)) {
	plot( log(-sort(sp500.logret.last1000)[1:m]), log((1:m)/nn), main=paste("m = ",m) )
}
par(mfrow=c(1,1))

# for particular case m=100
plot( log(-(sort(sp500.logret.last1000)[1:100])), log((1:100)/nn), type="p", pch=".",
	xlab="log(-x)", ylab="log(P(return < -x))", las=1 )
ex113.model <- lm(log((1:100)/nn) ~ log(-(sort(sp500.logret.last1000)[1:100])) )
lines( log(-(sort(sp500.logret.last1000)[1:100])), 
	(cbind(1, log(-(sort(sp500.logret.last1000)[1:100]))) %*% coef(ex113.model))[,1]	
	)
summary(ex113.model)
text(-3.1, -4, paste("slope =",round(coef(ex113.model)[2],3)))
text(-3.9, -6, paste("R^2 =",round(summary(ex113.model)$r.squared,3)))

