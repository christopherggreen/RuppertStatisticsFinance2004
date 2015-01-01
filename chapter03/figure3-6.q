############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure3-6.q                                              #
#                                                          #
############################################################

# ruppert does not provide any insight into how these
# plots were generated (i.e. what parameters) so we'll
# just use any old parameters

# generate random walks
p0    <- 1.75
drift <- 0
vol   <- 0.05
n     <- 1000
z     <- apply(
				rbind(rep(0,6),matrix(rnorm(6*n, drift, vol),ncol=6, nrow=n)), 
				2, 
				function(x) cumsum(x)
			)
series <- z
series[,c(1,3,5)]  <- log(p0) + z[,1:3]
series[,c(2,4,6)]  <- p0*exp(z[,4:6])

par(mfrow=c(3,2), xaxs="i", yaxs="i",lwd=3, pty="s", mar=c(3,3,3,3)+.1, las=1)

for ( i in 1:6 ) {
		plot(series[,i], type="l",  xlab="", ylab="", axes=F)
		axis(1, at=seq(0,1000,200))
		if ( i %% 2 ) {
			axis(2, at=seq(min(c(0,x)),max(c(3,x)),.5), adj=1)
			title(main="random walk",ylab="log price")
		}
		else {
			axis(2, at=seq(0,max(c(12,x)),2), adj=1)
			title(main="geometric random walk",ylab="price")
		}
		box()		
		NULL
}
par(mfrow=c(1,1))
