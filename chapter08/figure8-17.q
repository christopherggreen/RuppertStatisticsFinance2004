############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-17.q                                             #
#                                                          #
############################################################

# run figure8-14.q first
greeks.1 <- greeks.2 <- matrix(NA, nrow=length(tt), ncol=5)
for ( i in seq(along=tt) ) {
	greeks.1[i,] <-  unlist(greeks(stock.price.1[i], K, T.exp, tt[i], r, sigma, T, 0))
	greeks.2[i,] <-  unlist(greeks(stock.price.2[i], K, T.exp, tt[i], r, sigma, T, 0))
}

par(mfrow=c(2,1),lwd=2,xaxs="i",yaxs="i",las=1)	
plot(tt, greeks.1[,1], type="l", ylab="Delta", xlab="time")
plot(tt, greeks.1[,2], type="l", ylab="Theta", xlab="time")
par(mfrow=c(1,1))
