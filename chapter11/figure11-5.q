############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure11-5.q                                             #
#                                                          #
############################################################

hill.estimate <- function(x,c) { x <- x[-x > c]; c(length(x), 1/mean(log(-x/c))) }

cval  <- seq(0.01,0.03,length=50)
hills <- matrix(NA, ncol=2, nrow=50)
for ( i in seq(along=cval))
	hills[i,] <- hill.estimate(sp500.logret.last1000,cval[i]) 

par(lwd=3, xaxs="i", yaxs="i",las=1)
plot( hills, type="l", ylab="Hill estimator", xlab="n(c)", xlim=c(0,250), ylim=c(1.5,5.5), axes=F)
axis(1, at=seq(0,250,50))
axis(2, at=seq(1.5,5.5,0.5), adj=1)
box()
