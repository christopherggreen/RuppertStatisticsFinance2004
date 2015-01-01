############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-14.q                                             #
#                                                          #
############################################################

# option parameters
T.exp <- 1
sigma <- .1
r <- 0.06
S0 <- 100
K <- 100

# geometric Brownian motion
dt <- 0.01
tt <- seq(0,T.exp,dt)
z1    <- rnorm(length(tt)-1, r*dt, sigma*sqrt(dt))
z2    <- rnorm(length(tt)-1, r*dt, sigma*sqrt(dt))

stock.price.1 <- exp(cumsum(c(log(S0), z1)))
stock.price.2 <- exp(cumsum(c(log(S0), z2)))

par(mfrow=c(3,1),lwd=2,xaxs="i",yaxs="i",las=1)

# first plot: stock prices
plot(tt, stock.price.1, type="n", 
	xlab="",ylab="stock price", 
	ylim=range(c(stock.price.1,stock.price.2)))
lines(tt, stock.price.1, lty=1)
lines(tt, stock.price.2, lty=4)
abline(h=S0) 
grid.render(grids=list(list("x"=c(0.2,0.4,0.6,0.8),lty=2)))


# second plot: call prices
call.price.1 <- call.price.2 <- rep(NA, length(tt))
for ( i in seq(along=tt)) {
	call.price.1[i] <- optionprice.bs(stock.price.1[i], K, T.exp, tt[i], r, sigma, call=T, 0)
	call.price.2[i] <- optionprice.bs(stock.price.2[i], K, T.exp, tt[i], r, sigma, call=T, 0)
}
plot(tt, call.price.1, type="n", 
	xlab="",ylab="call price", 
	ylim=range(c(call.price.1,call.price.2)))
lines(tt, call.price.1, lty=1)
lines(tt, call.price.2, lty=4)
abline(h=S0) 
grid.render(grids=list(list("x"=c(0.2,0.4,0.6,0.8),lty=2),
	list("y"=c(2,4,6,8),lty=2) ))		
	
# third plot: put prices
put.price.1 <- put.price.2 <- rep(NA, length(tt))
for ( i in seq(along=tt)) {
	put.price.1[i] <- optionprice.bs(stock.price.1[i], K, T.exp, tt[i], r, sigma, call=F, 0)
	put.price.2[i] <- optionprice.bs(stock.price.2[i], K, T.exp, tt[i], r, sigma, call=F, 0)
}

plot(tt, call.price.1, type="n", 
	xlab="",ylab="put price", 
	ylim=range(c(put.price.1,put.price.2)))
lines(tt, put.price.1, lty=1)
lines(tt, put.price.2, lty=4)
abline(h=S0) 
grid.render(grids=list(list("x"=c(0.2,0.4,0.6,0.8),lty=2),
	list("y"=c(2,4),lty=2) ))	

par(mfrow=c(1,1))	 
