###########################################################
#                                                         #
# Copyright (c) Christopher G. Green, 2006                #
#                                                         #
###########################################################
############################################################
#                                                          #
# figure13-16.q                                            #
#                                                          #
############################################################

# run page416.q first
# this is figure 13.16

# Ruppert does not state how he did this, so 
# here's a rough hack

# recall mu2 = beta_lagdiff * lagdiff + beta_plusLD1 * plusLD1
# d(mu2)/d(lagdiff) = beta_lagdiff +
#                           beta_plusLD1 * (2*lagdiff*(lagdiff > 0))
#
# Roughly, what Ruppert did was compute the derivative of mu2, either
# using the above formula or some other means, then add 95% prediction
# intervals (using the estimated standard error, variance matrix of the coefficients, etc.)
#
# Here is a neat trick to do this easily. 
# The predict function (for linear models) has an argument pi.fit that will generate
# prediction intervals. So, if we can get predict to compute the derivative of mu2,
# we can get prediction intervals without any additional work.
#
# Since mu2 is a linear function of the model coefficients, this can be done rather
# easily---just predict at the "derivatives" of the regressors (zeroing out any that
# don't appear in the expression for mu2)

n <- length(lagdiff.eu01)

# 0 for the first two coefficients, 1 for the beta_lagdiff term, and 
# 2*lagdiff*(lagdiff > 0) for the beta_plusLD1 term
#
# this needs to be a matrix in order to zero out the intercept
# 
newdata.mu2.prime <- cbind(rep(0,n),rep(0,n),rep(1,n),2*pmax(lagdiff.eu01,0))

# predict will now compute the derivative of mu2 + 95% (simultaneous) prediction intervals
mu2.prime <- predict(bestcp.model.second, newdata=newdata.mu2.prime,pi.fit=T,conf.type="s")
ind <- order(lagdiff.eu01)

plot( lagdiff.eu01[ind], mu2.prime$fit[ind], 
	ylim=range(mu2.prime$fit,as.vector(mu2.prime$pi.fit)),
	ylab="derivative of mu_2", xlab="lagdiff", las=1 )
abline(h=0)
lines( lagdiff.eu01[ind], mu2.prime$pi.fit[ind,"upper"], lty=3)
lines( lagdiff.eu01[ind], mu2.prime$pi.fit[ind,"lower"], lty=3)

