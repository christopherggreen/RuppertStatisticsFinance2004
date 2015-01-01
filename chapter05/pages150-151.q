############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# pages150-151.q                                           #
#                                                          #
############################################################

# direct translation of MATLAB code on pages 150-151
# also creates figure 5.4

bmu    <- matrix(c(0.08,0.03,0.05), ncol=1)
bOmega <- matrix(c(0.3,0.02,0.01,0.02,0.15,0.03,0.01,0.03,0.18), nrow=3, ncol=3)
bone   <- matrix(rep(1,nrow(bmu)), ncol=1)

ibOmega <- solve(bOmega)
# need as.numeric to cast back to scalar
A <- as.numeric(t(bone) %*% ibOmega %*% bmu)
B <- as.numeric(t(bmu)  %*% ibOmega %*% bmu)
C <- as.numeric(t(bone) %*% ibOmega %*% bone)
D <- B*C - A^2

bg <- (B * ibOmega %*% bone - A * ibOmega %*% bmu )/D
bh <- (C * ibOmega %*% bmu  - A * ibOmega %*% bone)/D

gg <- t(bg) %*% bOmega %*% bg
hh <- t(bh) %*% bOmega %*% bh
gh <- t(bg) %*% bOmega %*% bh

mumin <- -gh/hh
sdmin <- sqrt(gg * (1 - (gh^2)/(gg*hh)))

muP    <- seq(min(bmu), max(bmu), length=50) # muP grid
sigmaP <- rep(0,50)                          # storage
for ( i in 1:50 ) {
	omegaP <- bg + muP[i]*bh
	sigmaP[i] <- sqrt(t(omegaP) %*% bOmega %*% omegaP)
}


ind  <- muP > mumin # indicates efficient frontier
ind2 <- muP < mumin # indicates locus below efficient frontier

# Create plot [figure 5.4] - efficient frontier is shown as a solid curve
# - the inefficient part of the locus is dashed

par(lwd=3, xaxs="e", yaxs="e",las=1)
xgrid=seq(0.25, 0.5, 0.05)
ygrid=seq(0.03,0.08,0.005)
plot(sigmaP, muP, type="n", axes=F, xlab="", ylab="")
lines(sigmaP[ind], muP[ind], lty=1)
lines(sigmaP[ind2], muP[ind2], lty=3)
points(sdmin, mumin, pch=16, cex=2)
mtext("standard deviation of return", side=1, line=2)
mtext("expected return", side=2, line=4)
grid.render(grid=list(x=xgrid,y=ygrid,lwd=1,lty=2))
axis(1, at=xgrid)
axis(2, at=ygrid, adj=1)
box()
