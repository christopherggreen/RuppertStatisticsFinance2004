############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure5-8.q                                              #
#                                                          #
############################################################

# direct translation of MATLAB code

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


ind  <- which(muP > mumin) # indicates efficient frontier
ind2 <- which(muP < mumin) # indicates locus below efficient frontier

rf.rate <- 0.02

# Create plot - efficient frontier is shown as a solid curve
# - the inefficient part of the locus is dashed

par(lwd=3, xaxs="e", yaxs="e",las=1)
xgrid=seq(0.00,0.50,0.05)
ygrid=seq(0.02,0.08,0.01)
plot(sigmaP, muP, type="n", axes=F, 
	xlab="", ylab="", xlim=range(xgrid),
	ylim=range(ygrid))
lines(sigmaP[ind], muP[ind], lty=1)
lines(sigmaP[ind2], muP[ind2], lty=3)
points(sdmin, mumin, pch=16, cex=2)
mtext("standard deviation of return", side=1, line=2)
mtext("expected return", side=2, line=4)
#grid.render(grid=list(x=xgrid,y=ygrid,lwd=1,lty=2))
axis(1, at=xgrid)
axis(2, at=ygrid, adj=1)
box()

srP <- (muP[ind] - rf.rate)/sigmaP[ind]
tan.ind <- ind[which(srP==max(srP))]
segments(0, rf.rate, sigmaP[tan.ind], muP[tan.ind], lty=3)
text(0+0.01,rf.rate+0.005,"F",cex=2)
text(sigmaP[tan.ind]+0.03,muP[tan.ind]-0.001,"T", cex=2)
pweights <- matrix(c(0,0.3,0.7),ncol=1)
pmu <- as.numeric(t(pweights) %*% bmu)
psigma <- as.numeric(sqrt(t(pweights) %*% bOmega %*% pweights))
text(psigma+0.03,pmu-0.001, "P", cex=2)
points(c(0,sigmaP[tan.ind],psigma),
	c(rf.rate,muP[tan.ind],pmu),pch=18)
