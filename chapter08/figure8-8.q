############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-8.q                                              #
#                                                          #
############################################################

# requires optionprice.bs.q and implied.volatility.q
#
sigma.seq <- seq(0.015,0.030,0.0005)
price.seq <- rep(NA, length(sigma.seq))
for ( i in seq(along=price.seq))
	price.seq[i] <- optionprice.bs(47.16,42.50,23,0,0.0491/253,sigma.seq[i],call=T,div=0)
par(lwd=2,xaxs="i",yaxs="i", las=1)
plot(sigma.seq, price.seq, type="l",
	xlab="sigma", ylab="price", lwd=3)
abline(h=5.3)
ivol <- implied.volatility(5.30,47.16,42.50,23,0,0.0491/253,T,0)$root
cat("Implied volatility:",ivol,"\n")
abline(v=ivol)
