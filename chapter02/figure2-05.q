############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-05.q                                             #
#                                                          #
############################################################
 
par(xaxs="e",yaxs="i",lwd=3,las=1)
x1 <- sort(rnorm(35, 0, 1))
# this just tries to match the range shown in ruppert's plot
x2 <- seq(min(-2.1,min(x1)), max(2.1, max(x1)), 0.1)
# ecdf is defined in ecdf.q
fnx <- ecdf(x1)
y1 <- fnx(x1)$y
y2 <- pnorm(x2, 0, 1)
plot( x1, y1, type="s", xlim=range(c(x1,x2)), ylim=range(c(y1,y2)),
	xlab="x", ylab="F(x)", axes=F)
lines(x2, y2, lty=3)
axis(1, at=c(-2,-1,0,1,2))
axis(2, at=seq(0,1.0,0.1), adj=1)
title("Empirical CDF")
box()
text( 0.50, 0.40, "F_n", cex=1.2, adj=0)
text(-0.90, 0.75, "F"  , cex=1.2, adj=0)
arrows( 0.45, 0.4, 0.25, fnx(0.25)$y , size=0.1)
arrows(-0.85, 0.7, 0   , pnorm(0,0,1), size=0.1)
