############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure4-6.q                                              #
#                                                          #
############################################################

n <- 400
mu <- 0
y0 <- 0
epsilons <- matrix(c(rnorm(n, 0, 1),y0),ncol=1)
powers   <- matrix(0, nrow=n+1, ncol=n+1)
index    <- seq(1,n+1)
right.tri <- lower.tri(powers,diag=T)[,rev(index)]
par(mfrow=c(3,1), lwd=2, xaxs="i", yaxs="e",las=1)

	powers[right.tri] <- 0.4^(outer(index, index, 
			function(x,y) abs(x - y))[,rev(index)])[right.tri]
	series <- as.vector(powers %*% epsilons)
	plot(series,type="l",xlab="",ylab="")
	title(paste("ARIMA(1,0,0) with mu = ",mu,"and phi = ",0.4))
	
	series.integral <- cumsum(series)
	plot(series.integral,type="l",xlab="",ylab="")
	title(paste("ARIMA(1,1,0)"))
	
	series.integral.integral <- cumsum(series.integral)
	plot(series.integral.integral,type="l",xlab="",ylab="")
	title(paste("ARIMA(1,2,0)"))

par(mfrow=c(1,1))
