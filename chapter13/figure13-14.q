############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-14.q                                            #
#                                                          #
############################################################

# run page416.q first
# need to put things in order to plot
# this is figure 13.14
par(mfrow=c(2,1), lwd=3)
mu1.plot <- mu1[order(rate.eu01[-1])]
mu2.plot <- mu2[order(lagdiff.eu01)]
plot( sort(rate.eu01[-1]), mu1.plot, type="l", xlab="rate", ylab="mu1")
plot( sort(lagdiff.eu01), mu2.plot, type="l", xlab="lagdiff", ylab="mu2")
par(mfrow=c(1,1))
