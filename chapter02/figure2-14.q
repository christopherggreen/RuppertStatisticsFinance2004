############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-14.q                                             #
#                                                          #
############################################################
 
# note: ifelse is a vectorized if-else: for each element if the condition
# is true, the corresponding element of the second argument is used, 
# otherwise the corresponding element of the third element is used
#
# do not confuse if...else with ifelse, as they work differently
#
prior <- function(theta) ifelse(0 <= theta & theta <= 1, 6*theta*(1-theta), NA)

posterior <- function(theta, y) {
		g <- function(theta,y) prior(theta) * dbinom(y, 3, theta)
		# integrate the prior against the density as long
		# as the parameter theta is within its range [0,1]
		ifelse(0 <= theta & theta <= 1,
			g(theta,y)/integrate(g, 0, 1, y=y)$integral,
			NA
		)					
}

th <- seq(0,1,0.01)
pr.th <- prior(th) 
# this step takes a little while
po.th <- posterior(th,y=3)
par(xaxs="e", yaxs="e", lwd=3, cex=1.2)
plot(th, po.th, xlab="x", ylab="density", type="l", 
	xlim=c(0,1), ylim=range(c(pr.th,po.th)))
lines(th, pr.th, lty=3)
text(0.1, 1.6, "prior", adj=0)
text(0.5, 2  , "posterior", adj=0)
arrows(0.2, 1.6, 0.38, prior(0.38), size=.1)
arrows(0.6, 2  , 0.7, posterior(0.7,3), size=.1)

