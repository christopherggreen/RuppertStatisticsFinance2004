############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-9.q                                             #
#                                                          #
############################################################

x <- seq(0,3,length=100)
plot(x, ifelse(x > 1, x - 1, 0), lwd=3, type="l", xlab="", ylab="")
lines(x, ifelse(x > 1, 1, 0), lty=3)
key(corner=c(0,1), border=T, lines=list(lty=c(1,3)), 
	text=list(c("plus fn.", "derivative")))
