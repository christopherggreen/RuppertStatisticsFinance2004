############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################

"optionprice.bs" <- 
#
# computes the price of an option using Black-Scholes
# Christopher G. Green, 2003
#	
# stock.price			current price of the stock
# strike.price		    strike price of the option
# time.exp				expiry time of the option
# time.cur				current time
# rf					risk-free rate (continuously compounded)
# vol					volatility
# call					T if this is a call, F for a put (default = T)
# div 					dividend rate (continuously compounded, default = 0)
#
function(stock.price,strike.price,time.exp,time.cur,rf,vol,call=T,div=0)
{
	dt = time.exp - time.cur
	
	# compute d+ and d- quantities	
	d.pos <- log(stock.price/strike.price) + (rf - div + 0.5*vol^2)*dt
	d.pos <- d.pos / (vol * dt^0.5)
	d.neg <- d.pos - vol*dt^0.5

	# compute the price of a call
	price.call <- stock.price*exp(-div*dt)*pnorm(d.pos) - 
		strike.price*exp(-rf*dt)*pnorm(d.neg)
	
	if (call)
	{ 
		price.call
	}
	else
	{
		# compute the price of a put using put call parity
		price.call - stock.price*exp(-div*dt) + strike.price*exp(-rf*(time.exp-time.cur)) 
	}
}
