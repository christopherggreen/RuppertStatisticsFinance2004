############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################

"implied.volatility" <- 
#
# computes the implied volatility of an option using Black-Scholes
# Christopher G. Green, 2006
#	
# stock.price			current price of the stock
# strike.price  		strike price of the option
# time.exp				expiry time of the option
# time.cur				current time
# rf					risk-free rate (continuously compounded)
# call					T if this is a call, F for a put (default = T)
# div 					dividend rate (continuously compounded, default = 0)
#
# needs optionprice.bs
#
function(real.price, stock.price,strike.price,time.exp,time.cur,rf,call=T,div=0)
{
		bsfunc <- substitute(function(x) optionprice.bs(A1,A2,A3,A4,A5,x,A6,A7) - A8,
			list("A1"=stock.price, "A2"=strike.price, "A3"=time.exp, "A4"=time.cur,
				"A5"=rf, "A6"=call, "A7"=div, "A8"=real.price))
		uniroot(bsfunc, c(0, 1000))[c("root","f.root")]
}
