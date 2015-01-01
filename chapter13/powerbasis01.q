############################################################
#                                                          #
# powerbasis01.q                                           #
#                                                          #
############################################################

# direct translation of Ruppert's MATLAB code

"powerbasis01" <- 
#
#	Returns the power basis functions of a spline of given degree
#	USAGE: xm = powerbasis(x,degree,knots) 
#
#	Last edit: 	3/22/99
#
function(x, degree, knots, der=0) {

	if (der > degree)
		stop("der > degree")
	if ( degree <= 0 )
		stop("degree must be positive")
	if ( der < 0 )
		stop("der must be non-negative")
	degree <- as.integer(degree)
	der <- as.integer(der)
	x <- as.matrix(x)
	n <- nrow(x)
	nknots <- length(knots)

	xm <- matrix(0, ncol=1+degree+nknots, nrow=n)

	if ( der == 0 ) 
		xm[,1:(degree+1)] <- outer(x,0:degree,FUN="^")
	else {
		xm[,1+(der:degree)] <- outer(x,0:(degree-der),FUN="^")
		for ( i in der:degree )
			xm[,i+1] <- prod( (i-der+1):i) * xm[,i+1]
	}
	if (nknots > 0) 
		for ( i in 1:nknots )
			xm[x > knots[i],degree+1+i] <- (if (der==0) 1 else prod( (degree-der+1):degree )) * 
				((x[x > knots[i]] - knots[i])^(degree-der))

	
			
		xm
}
