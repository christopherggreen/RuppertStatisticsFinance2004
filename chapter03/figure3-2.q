############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure3-2.q                                              #
#                                                          #
############################################################

s0    <- 0
drift <- 0.5
vol   <- 1
t0    <- 0
tmax  <- 10
dt    <- 0.01
tt    <- seq(t0,tmax,dt)

# matrix of upper and lower bounds for the process
bounds <- matrix(rep(s0 + drift * tt,2), ncol=2) + 
	(matrix(rep(vol*sqrt(tt),2), ncol=2) %*% diag(c(1,-1)))
	
par(xaxs="i", yaxs="i", lwd=3, las=1)
plot(tt, s0 + drift * tt, type="l", xlab="time", ylab="", ylim=c(-1,9))
lines(tt, bounds[,1], lty=3)
lines(tt, bounds[,2], lty=3)
title("random walk")
key(4,7.9,corner=c(0,0), border=T, lines=list(lty=c(1,3,3)), 
	text=list(c("mean","mean + SD","mean - SD")))	

