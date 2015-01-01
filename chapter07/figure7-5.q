############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure7-5.q                                              #
#                                                          #
############################################################

# run page247.q first

# penalized spline code doesn't appear until chapter 13
# so we'll just use S-Plus's smooth.spline function
# for illustration purposes

plot( capm.logret.monthly$sp500.logret, capm.logret.monthly$ford.logret, type="p", 
	xlab="S&P excess log return", ylab="Ford excess log return")
abline(h=0,lty=2)
abline(v=0,lty=2)
lines(smooth.spline( capm.logret.monthly$sp500.logret, capm.logret.monthly$ford.logret ), lty=1 )
lines(capm.logret.monthly$sp500.logret, predict(model.2), lty=3)
lines(capm.logret.monthly$sp500.logret, predict(model.3), lty=4)
