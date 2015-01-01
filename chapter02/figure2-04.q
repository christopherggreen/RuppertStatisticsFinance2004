############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-04.q                                             #
#                                                          #
############################################################
 
# see figure2-03.q for comments
par(xaxs="e", yaxs="e", lwd=3, las=1)
x <- seq(0, 18, 0.1)
plot(x, dlnorm(x, 1, 0.5), type="l", 
	xlab="x", ylab="density", axes=F)
lines(x, dlnorm(x, 1, 1), lty=2)
lines(x, dlnorm(x, 1.5, 0.5), lty=3)
box()
axis(1, at=seq(0,18,2))
axis(2, at=seq(0,0.35,0.05))
text(4, 0.27, "mu = 1, sigma = 0.5", adj=0)
text(2, 0.03, "mu = 1, sigma = 1", adj=0.5)
text(8, 0.17, "mu = 1.5, sigma = 0.5", adj=0)
arrows( 3.9, 0.27, 3.5, dlnorm(3.5,1.0,0.5), size=.1)
arrows( 1.9, 0.03, 3.9, dlnorm(3.9,1.0,1.0), size=.1)
arrows( 7.9, 0.17, 6.1, dlnorm(6.1,1.5,0.5), size=.1)
