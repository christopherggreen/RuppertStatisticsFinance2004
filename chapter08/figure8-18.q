############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-18.q                                             #
#                                                          #
############################################################

S0.seq <- seq(85,110,.1)
price.seq <- rep(NA, length(S0.seq))
for ( i in seq(along=price.seq) ) {
	price.seq[i] <- optionprice.bs(S0.seq[i],100,0.25,0,0.06,0.1)
}

par(lwd=2,xaxs="i",yaxs="i",las=1)
plot(S0.seq, price.seq, type="l", 
	lwd=2, xlab="S0", ylab="price of call")
lines(S0.seq, pmax(S0.seq-K,0), lty=3, lwd=2)
lines(S0.seq, pmax(S0.seq-K*exp(-0.06*0.25),0), lty=4, lwd=2)
key(93,10,corner=c(0,0),border=T,
	text=list(c("price of European call","intrinsic value","adj. intrinsic value")),
	lines=list(lty=c(1,3,4)))


