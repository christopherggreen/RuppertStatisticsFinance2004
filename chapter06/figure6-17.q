############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-17.q                                             #
#                                                          #
############################################################

# run page212.q first

# the SAS code corresponds to ordinary residuals
# see figure6-15.q for example of how to use studentized residuals
# also note the scale is now 10 = sqrt(100)

qqplot.matlab( 10*abs(resid(fit.nls.tbs)), 
	xlabels=seq(-0.2,0.2,0.05),
	qlabels=c(0.02,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.98),
	xlab="fitted values")	