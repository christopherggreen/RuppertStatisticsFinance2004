############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################

greeks <- 
# computes the greeks for an option using Black-Scholes
# Christopher G. Green, 2003

# stock.price		current price of the stock
# strike.price		strike price of the option
# time.exp			expiry time of the option
# time.cur			current time
# rf					risk-free rate
# vol					volatility
# call					T if this is a call, F for a put (default = T)
# div 					dividend rate (default = 0)
function(stock.price,strike.price,time.exp,time.cur,rf,vol,call=T,div=0)
{

	dt = time.exp - time.cur		# time remaining 
	dsc = exp(-div*dt)				# discount factor
	iscall = as.numeric(!call)	# 1 if put option, 0 if call option
	sgncall = (-1)^iscall			# -1 if put, 1 if call
	
	# compute d+ and d- quantities	
	d.pos <- log(stock.price/strike.price) + (rf - div + 0.5*vol^2)*dt
	d.pos <- d.pos / (vol * dt^0.5)
	d.neg <- d.pos - vol*dt^0.5

	# compute greeks
	delta <- dsc*(pnorm(d.pos)-iscall)
	
	theta <- -(stock.price*dnorm(d.pos)*vol*dsc)/(2*sqrt(dt))
	theta <- theta + sgncall*div*stock.price*pnorm(sgncall*d.pos)*dsc
	theta <- theta - sgncall*rf*strike.price*exp(-rf*dt)*pnorm(sgncall*d.neg)
	
	gamma <- (dnorm(d.pos)*dsc)/(stock.price*vol*sqrt(dt))

	vega <- stock.price*sqrt(dt)*dnorm(d.pos)*dsc

	rho <- sgncall*strike.price*dt*dsc*pnorm(sgncall*d.neg)

	list(delta=delta,theta=theta,gamma=gamma,vega=vega,rho=rho)
}
