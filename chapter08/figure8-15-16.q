############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-15-16.q                                          #
#                                                          #
############################################################

# run figure8-14.q first
plot( tt[-1], diff(log( call.price.1 ))/diff(log(stock.price.1)),
	type="l", xlab="time", ylab="call return/stock return" )
plot( tt[-1], diff(log( call.price.2 ))/diff(log(stock.price.2)),
	type="l", xlab="time", ylab="call return/stock return" )
plot( tt[-1], diff(log( put.price.1 ))/diff(log(stock.price.1)),
	type="l", xlab="time", ylab="put return/stock return" )
plot( tt[-1], diff(log( put.price.2 ))/diff(log(stock.price.2)),
	type="l", xlab="time", ylab="put return/stock	 return" )	
