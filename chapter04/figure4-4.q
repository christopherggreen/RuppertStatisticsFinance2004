############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure4-4.q                                              #
#                                                          #
############################################################

n  <- 1000
mu <- 0
y0 <- 0
epsilons <- matrix(c(rnorm(n, 0, 1),y0),ncol=1)
powers   <- matrix(0, nrow=n+1, ncol=n+1)
index    <- seq(1,n+1)
right.tri <- lower.tri(powers,diag=T)[,rev(index)]
par(mfrow=c(1,2), lwd=2, xaxs="i", yaxs="e",las=1)
for ( phi in c(0.9,1) ) {
	powers[right.tri] <- phi^(outer(index, index, 
			function(x,y) abs(x - y))[,rev(index)])[right.tri]
	plot(
		as.vector(powers %*% epsilons),
		type="l", 
		xlab="", 
		ylab=""
	)
	title(paste("AR(1): phi = ",phi))
}
par(mfrow=c(1,1))