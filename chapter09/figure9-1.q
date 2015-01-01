############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure9-1.q                                              #
#                                                          #
############################################################

# this is just a repeat of the end of yield.q

par(lwd=3,xaxs="i",yaxs="i")
plot(rr,value, type="l", xlab="yield to maturity",
	ylab="price of bond", main=paste("par=",parval,"coupon payment=",CC,"TT=",TT),
	xlim=c(lower,upper),ylim=range(value))
abline(v=yield2M)
abline(h=price)
