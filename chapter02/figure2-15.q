############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-15.q                                             #
#                                                          #
############################################################
 
# run figure2-14.q first

posterior.cdf <- function(theta, y) {
	nt <- length(theta)
	result <- rep(NA,theta)
	for ( i in seq(1,nt) ) {
		result[i] <- integrate(posterior, 0, z, y=3)$integral
	}
	result		
}

# this step takes a while
po.cdf.th <- posterior.cdf(th,3)


par(xaxs="i", yaxs="i", lwd=3)
plot(th, po.cdf.th, type="l", xlab="x", ylab="P(theta < x)",
	axes=F)

axis(1, at=seq(0,1,0.1))
axis(2, at=seq(0,1,0.1), adj=1)
box()

# find quantiles
# uniroot is used to find roots of equations
lower.quantile <- uniroot(
	function(x) posterior.cdf(x) - 0.05, 
	c(0, 1))$root
upper.quantile <- uniroot(
	function(x) posterior.cdf(x) - 0.95, 
	c(0, 1))$root
segments(0, 0.05, lower.quantile, 0.05, lwd=1)
segments(0, 0.95, upper.quantile, 0.95, lwd=1)
segments(lower.quantile, 0, lower.quantile, 0.05, lwd=1)
segments(upper.quantile, 0, upper.quantile, 0.95, lwd=1)

text(0.35, 0.5, "Posterior\nCDF")
arrows(0.45, 0.5, 0.8, posterior.cdf(0.8,3), size=.1)
text(0.75, 0.3, ".95\nquantile")
text(0.60, 0.1, ".05\nquantile")
arrows(0.83, 0.25, upper.quantile, 0, size=.1)
arrows(0.55, 0.10, lower.quantile, 0, size=.1)
