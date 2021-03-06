############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure4-2.q                                              #
#                                                          #
############################################################

n <- 200
mu <- 0
y0 <- 0
epsilons <- matrix(c(rnorm(n, 0, 1),y0),ncol=1)
powers   <- matrix(0, nrow=n+1, ncol=n+1)
index    <- seq(1,n+1)
# little trick to get the right powers of phi into 
# the matrix
# get indices of the lower triangle, but reverse
# the order of the columns
right.tri <- lower.tri(powers,diag=T)[,rev(index)]
par(mfrow=c(2,2), lwd=2, xaxs="i", yaxs="e",las=1)
for ( phi in c(0.9,-0.6,1,1.02) ) {
	# powers is a matrix, but it can be indexed as a 
	# long vector (stack the columns)
	#
	# outer applies a function to all pairs of elements
	# in two vectors
	#
	# so for each pair of indices compute |x-y|
	# reverse the order of the columns, then use
	# the elements as exponents
	#
	# extract the triangle we need for the powers matrix
	#
	powers[right.tri] <- (phi^(outer(index, index, 
			function(x,y) abs(x - y))[,rev(index)]))[right.tri]
	plot(
		as.vector(powers %*% epsilons),
		type="l", 
		xlab="", 
		ylab=""
	)
	title(paste("AR(1): phi = ",phi))
}
par(mfrow=c(1,1))
