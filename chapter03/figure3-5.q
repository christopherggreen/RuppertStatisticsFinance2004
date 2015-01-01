############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure3-5.q                                              #
#                                                          #
############################################################

# run page85.q first

# generate random walks
p0    <- price[1]
drift <- mean(logreturn)
vol   <- stdev(logreturn)
n     <- 250
z     <- rbind(rep(0,5),matrix(rnorm(5*n, drift, vol),ncol=5, nrow=n))
grw   <- p0*exp(apply(z, 2, function(x) cumsum(x)))

par(mfrow=c(3,2), xaxs="i",yaxs="i",lwd=3, pty="s", mar=c(3,1,2,1)+.1, las=1)

for ( i in seq(ncol(grw)) ) {
		plot(grw[,i], type="l", main="geometric random walk", xlab="",
			ylab="", axes=F)
		axis(1, at=seq(0,250,50))
		axis(2, at=seq(0,max(c(100,x)),5), adj=1)
		box()
		NULL
	}

plot(price, xlab="",ylab="price", type="l", axes=F, ylim=c(40,60))
title("GE, daily - 12/17/99 to 12/15/00")
axis(1, at=seq(0,250,50))
axis(2, at=seq(40,60,5), adj=1)
box()

par(mfrow=c(1,1))
