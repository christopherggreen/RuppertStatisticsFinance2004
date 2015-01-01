############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# ecdf.q                                                   #
#                                                          #
############################################################

# returns a function that computes the empirical CDF of x
# note the return value is a *function*

ecdf <- function(x) {
	x  <- as.vector(x)
	# drop NA's and NaN's
	x  <- sort(x[is.finite(x)])
	fx <- factor(x)
	lx <- levels(fx)
	tx <- tabulate(as.numeric(fx),length(lx))
	xx <- c(-.Machine$double.xmax, as.numeric(lx), 
		.Machine$double.xmax)
	yy <- c(0,cumsum(tx/sum(tx)), 1)
	substitute(
		function(z) {
			approx(X, Y, xout=z, method="constant")
		},
		frame=list(X=xx,Y=yy)
	)
}
