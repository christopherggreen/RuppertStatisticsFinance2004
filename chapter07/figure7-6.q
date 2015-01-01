############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure7-6.q                                              #
#                                                          #
############################################################

par(lwd=2,xaxs="i",yaxs="i",las=1)
plot( cumprod(1+capm.logret.monthly$ford.logret), type="n", 
	xlab="time in months since 3/96",
	ylab="gross return since t=1",
	ylim=c(0.5,3.0))
lines( cumprod(1+capm.logret.monthly$sp500.logret), lty=1 )
lines( cumprod(1+capm.logret.monthly$ford.logret), lty=3 )
key(corner=c(1,0),border=T,text=list(c("S & P","Ford")),lines=list(lty=c(1,3)))
