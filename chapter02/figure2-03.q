############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-03.q                                             #
#                                                          #
############################################################
 
# plotting parameters
par(xaxs="e", yaxs="e", lwd=3, las=1)
x <- seq(-5, 5, 0.1)

# plot first density
plot(x, dnorm(x, 0, 1), type="l", 
	xlab="x", ylab="density", axes=F)
# add others
lines(x, dnorm(x, 1, 1), lty=2)
lines(x, dnorm(x, 0, 2), lty=3)
box()
axis(1, at=c(-5,0,5)      )
axis(2, at=seq(0,0.4,0.05))

# annotations
text( 4, 0.32, "mu = 1, sigma = 1", adj=0)
text(-4, 0.12, "mu = 0, sigma = 2", adj=0.5)
text(-3.5, 0.31, "mu = 0, sigma = 1", adj=0)
arrows( 3.9, 0.32,  2, dnorm( 2,1,1), size=.1)
arrows(-4, 0.10, -3.5, dnorm(-3.5,0,2), size=.1)
arrows(-3, 0.29, -1, dnorm(-1,0,1), size=.1)
