############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure5-9.q                                              #
#                                                          #
############################################################

# run page162.q script first
xgrid <- seq(0,0.6,0.1)
ygrid <- c(rf.rate,seq(0.04,0.08,0.005))
par(lwd=3, xaxs="i", yaxs="i",las=1)
plot(sigmaP[Ieff], muP[Ieff], type="l", 
	xlab="",ylab="", axes=F, xlim=range(xgrid),
	ylim=range(ygrid))
points(sigmaP[Itangency], muP[Itangency], pch=18, cex=2)
points(sigmaP[imin], muP[imin], pch=1, cex=2)
points(0, rf.rate, pch="x", cex=2)
segments(0, rf.rate, sigmaP[Itangency], muP[Itangency])
mtext("standard deviation of return", side=1, line=2)
mtext("expected return", side=2, line=4)
axis(1, at=xgrid)
axis(2, at=ygrid, adj=1)
box()

text(.35,.045, "Minimum variance portfolio", adj=0)
text(.40,.065, "Tangency portfolio", adj=0)
text(.025,.055,"Risk-free asset", adj=0)

arrows(.350,.045, sigmaP[imin]-.002, muP[imin]-.002)
arrows(.400,.065, sigmaP[Itangency]-.002, muP[Itangency]-.002)
arrows(.023,.053, 0, rf.rate)
