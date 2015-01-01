############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-19.q                                             #
#                                                          #
############################################################

y <- c(runif(25, 0.75, 1.25), rlnorm(25,log(5),0.1))

par(xaxs="i",yaxs="i", las=1, lwd=3)
plot( seq(0.1,8,0.1), log(seq(0.1,8,0.1)), type="l",
	xlab="Y", ylab="log(Y)", xlim=c(0,8), 
	ylim=c(-2.5,2.5) )
ymin <- rep(par()$usr[3],length(y))
points( y, ymin )
points( y, log(y) )
segments( y, ymin, y, log(y), lty=3)
segments( 0, log(y), y, log(y), lty=3)
# tangent lines
abline(-1,1)
abline(log(5)-1,1/5)

text(0.3, 2.2, "tangent line at Y=1", adj=0)
text(3.3, 0.4, "tangent line at Y=5", adj=0)

arrows(1, 2.1, 3, 2, lwd=3)
arrows(3.5, 0.5, 3.7, log(5)-1+(3.7/5), lwd=3)
