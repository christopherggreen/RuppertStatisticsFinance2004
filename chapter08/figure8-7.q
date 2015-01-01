############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-7.q                                              #
#                                                          #
############################################################

# requires optionprice.bs.q
#
# parameters for the computations
# initial stock price
S0    <- 100
# continuously compounded risk-free rate
r     <- 0.06
# volatility
sigma <- 0.1
# time to expiry in years
T.exp <- 0.25
# strike price
K     <- 100*exp(r*T.exp)

# plotting parameters
par(mfrow=c(2,3),lwd=2,xaxs="i",yaxs="i",pty="s",las=1)

# sequence of volatilities
sigma.seq <- seq(0,0.2,0.01)
price.seq <- rep(NA, length(sigma.seq))
for ( i in seq(along=price.seq) )
	price.seq[i] <- optionprice.bs(S0, K, T.exp, 0, r, sigma.seq[i], call=T, div=0) 
plot(sigma.seq, price.seq, type="l", xlab="sigma", ylab="price of call")

# sequence of strikes
strike.seq <- seq(90,110)
price.seq  <- rep(NA, length(strike.seq))
for ( i in seq(along=price.seq) )
	price.seq[i] <- optionprice.bs(S0,strike.seq[i],T.exp,0,r,sigma,call=T,div=0)
plot(strike.seq, price.seq, type="l",xlab="K",ylab="",ylim=c(0,15))

# sequence of initial prices
snaught.seq <- strike.seq
price.seq  <- rep(NA, length(strike.seq))
for ( i in seq(along=price.seq) )
	price.seq[i] <- optionprice.bs(snaught.seq[i],K,T.exp,0,r,sigma,call=T,div=0)
plot(snaught.seq, price.seq, type="l",xlab="S0",ylab="",ylim=c(0,15))

# sequence of interest rates
r.seq <- seq(0,0.12,0.01)
price.seq <- rep(NA, length(r.seq))
for ( i in seq(along=price.seq) )
	price.seq[i] <- optionprice.bs(S0,K,T.exp,0,r.seq[i],sigma,call=T,div=0)
plot(r.seq, price.seq, type="l",xlab="r",ylab="price of call",ylim=c(1,3))

# sequence of times to expiry
T.seq <- seq(0,0.12,0.01)
price.seq <- rep(NA, length(T.seq))
for ( i in seq(along=price.seq) )
	price.seq[i] <- optionprice.bs(S0,K,T.seq[i],0,r,sigma,call=T,div=0)
plot(T.seq, price.seq, type="l",xlab="T",ylab="",ylim=c(0,1.5))

par(mfrow=c(1,1))
