############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-16.q                                             #
#                                                          #
############################################################

# run page212.q first

# the SAS code corresponds to ordinary residuals
# see figure6-14.q for example of how to use studentized residuals
# also note the scale is now 10 = sqrt(100)

plot( 10*predict(fit.nls.tbs), 10*abs(resid(fit.nls.tbs)), xlab="fitted values",
	ylab="absolute residual" )	
lines(lowess(10*predict(fit.nls.tbs), 10*abs(resid(fit.nls.tbs))))