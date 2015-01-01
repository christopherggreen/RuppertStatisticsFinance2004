###########################################################
#                                                         #
# Copyright (c) Christopher G. Green, 2006                #
#                                                         #
###########################################################
############################################################
#                                                          #
# figure5-6.q                                              #
#                                                          #
############################################################

# run page162.q script first

xgrid <- seq(0.25,0.60,0.05)
ygrid <- seq(0.03,0.08,0.01)
par(lwd=3,xaxs="e",yaxs="e",las=1)
plot(sigmaP[Ieff], muP[Ieff], type="l", 
	xlab="",ylab="", axes=F, xlim=range(xgrid),
	ylim=range(c(bmu,ygrid)))
text(sqrt(diag(bOmega)),bmu,c("1","2","3"), cex=2, adj=0)
segments(sigmaP[Itangency], muP[Itangency])
mtext("standard deviation of return (sigma_p)", side=1, line=2)
mtext("expected return (mu_p)", side=2, line=4)
axis(1, at=xgrid)
axis(2, at=ygrid, adj=1)
grid.render(grid=list(x=xgrid,y=ygrid,lty=2,lwd=1))
box()


# now add the unconstrained frontier from figure 5-4

bmu2    <- matrix(c(0.08,0.03,0.05), ncol=1)
bOmega2 <- matrix(c(0.3,0.02,0.01,0.02,0.15,0.03,0.01,0.03,0.18), nrow=3, ncol=3)
bone2   <- matrix(rep(1,nrow(bmu2)), ncol=1)

ibOmega2 <- solve(bOmega2)
# need as.numeric to cast back to scalar
A2 <- as.numeric(t(bone2) %*% ibOmega2 %*% bmu2)
B2 <- as.numeric(t(bmu2)  %*% ibOmega2 %*% bmu2)
C2 <- as.numeric(t(bone2) %*% ibOmega2 %*% bone2)
D2 <- B2*C2 - A2^2

bg2 <- (B2 * ibOmega2 %*% bone2 - A2 * ibOmega2 %*% bmu2 )/D2
bh2 <- (C2 * ibOmega2 %*% bmu2  - A2 * ibOmega2 %*% bone2)/D2

gg2 <- t(bg2) %*% bOmega2 %*% bg2
hh2 <- t(bh2) %*% bOmega2 %*% bh2
gh2 <- t(bg2) %*% bOmega2 %*% bh2

mumin2 <- -gh2/hh2
sdmin2 <- sqrt(gg2 * (1 - (gh2^2)/(gg2*hh2)))

muP2    <- seq(min(bmu2), max(bmu2), length=50) # muP grid
sigmaP2 <- rep(0,50)                          # storage
for ( i in 1:50 ) {
	omegaP2 <- bg2 + muP2[i]*bh2
	sigmaP2[i] <- sqrt(t(omegaP2) %*% bOmega2 %*% omegaP2)
}


ind2  <- muP2 > mumin2 # indicates efficient frontier
sigmaP2[ind2]
muP2[ind2]
lines(sigmaP2[ind2], muP2[ind2], lty=3, lwd=3)

key(par()$usr[2]-0.01, par()$usr[3]+0.001, corner=c(1,0), border=T,
	lines=list(lty=c(1,2)), 
	text=list(c("no negative wts","unconstrained wts")))
	
	
