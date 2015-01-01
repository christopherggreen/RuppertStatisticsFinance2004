############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure3-3.q                                              #
#                                                          #
############################################################

drift <- 0.1
vol   <- 0.2
t0    <- 0
tmax  <- 20
dt    <- 1
tt    <- seq(t0, tmax, dt)
p0    <- 100
# increments
z1    <- rnorm(length(tt)-1, drift, vol)
z2    <- rnorm(length(tt)-1, drift, vol)

# compute log price by cumulatively summing up increments
log.price.1 <- cumsum(c(log(p0), z1))
log.price.2 <- cumsum(c(log(p0), z2))

par(mfrow=c(3,1),xaxs="i",yaxs="i",lwd=2, las=1)
plot(tt[-1], z1, type="l", xlim=c(-5,20), ylim=c(-0.5,1),
	xlab="year", ylab="log return")
lines(tt[-1], z2, lty=4)
segments(0,vol,20,vol, lwd=4)
title(main=paste("log returns are N(",drift,", (",vol,")^2 )"))
key(-4.9, 0.5, corner=c(0,0), border=T,
	lines=list(lty=c(1,4,1),lwd=c(2,2,4)),
	text=list(c("GRM","GRM","median")))

plot(tt, log.price.1, type="l", xlim=c(-5,20), ylim=c(4,8),
	xlab="year", ylab="log price")
lines(tt, log.price.2, lty=4)
segments(0,log(p0),20,log(p0) + (tmax*drift), lwd=4)
key(-4.9, 6.5, corner=c(0,0), border=T,
	lines=list(lty=c(1,4,1),lwd=c(2,2,4)),
	text=list(c("GRM","GRM","median")))
	
plot(tt, exp(log.price.1), type="l", xlim=c(-5,20), ylim=c(0,2000),
	xlab="year", ylab="price")
lines(tt, exp(log.price.2), lty=4)
lines(tt, exp(log(p0) + (tt*drift)), lwd=4)
lines(tt, exp(log(p0) + (tt*drift) + 0.5*(tt*(vol^2))), lty=3)
key(-4.9, 1000, corner=c(0,0), border=T,
	lines=list(lty=c(1,4,1,3),lwd=c(2,2,4,2)),
	text=list(c("GRM","GRM","median","mean")))
		
par(mfrow=c(1,1))
