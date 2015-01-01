############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-13.q                                             #
#                                                          #
############################################################
 
# Ruppert isn't specific about distributions for
# figure 2.13, so we'll just assume bivariate normal
# with  marginal means 0 and marginal variances 1
# won't match what Ruppert has exactly

par(mfrow=c(4,2), xaxs="e", yaxs="e", lwd=3, cex=1.2, las=1)

for ( rho in c(0.01, 0.25, 0.5, 0.95, 0.11, 0.83, -0.89, -1) ) {
	plot(rmvnorm(500, mean=c(0,0), rho=rho), xlab="x", ylab="y",
		xlim=c(-4,4), ylim=c(-4,4), pch="*" )
	text(-3,4, paste("r =",rho), cex=1)
}
par(mfrow=c(1,1))
