############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-15.q                                             #
#                                                          #
############################################################

# run figure6-14.q first

# again, figure 6.15 differs from what the SAS code suggests
qqplot.matlab( 100*resid(fit.nls), xlabels=seq(-0.6,0.6,0.2),
	qlabels=c(0.02,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.98),
	xlab="residual")
	
qqplot.matlab(fit.nls.studres, xlabels=seq(-5,5,1),
	qlabels=c(0.02,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.98),
	xlab="residual")