############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-15.q                                            #
#                                                          #
############################################################

# run page416.q first
par(mfrow=c(2,1), lwd=3)
mu1.plot <- mu1[order(rate.eu01[-1])]
mu2.plot <- mu2[order(lagdiff.eu01)]
plot( rate.eu01, diff.eu01, type="p", pch=".", 
	xlab="rate", ylab="difference")
lines( sort(rate.eu01[-1]), mu1.plot)
plot( diff.eu01[-1], lagdiff.eu01, type="p", pch=".", 
	xlab="lagdiff", ylab="differnce")
lines( sort(lagdiff.eu01), mu2.plot)
par(mfrow=c(1,1))
