############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page319-320.q                                            #
#                                                          #
############################################################

# load data
usstrips <- read.table("../common/data/strips_dec95.txt", skip=3, header=F)
names(usstrips) <- c("time2mat","price")
# sort by time2mat so things plot correctly
usstrips <- usstrips[order(usstrips$time2mat),]

nls.model.linear <- nls( price ~ 100*exp(-beta0*time2mat - 0.5*(beta1*(time2mat^2))), 
	data=usstrips, start=list(beta0=.01,beta1=-.005) )	
	
summary(nls.model.linear)$parameters
# 95% confidence intervals
cbind(
	coef(nls.model.linear) + qnorm(.025)*summary(nls.model.linear)$parameters[,"Std. Error"],
	coef(nls.model.linear) - qnorm(.025)*summary(nls.model.linear)$parameters[,"Std. Error"]
)

nls.model.quadratic <- nls( price ~ 100*exp(-beta0*time2mat - 0.5*(beta1*(time2mat^2)) - (beta2*(time2mat^3))/3), 
	data=usstrips, start=list(beta0=.01,beta1=-.005,beta2=-.005) )	
	
summary(nls.model.quadratic)$parameters
# 95% confidence intervals
cbind(
	coef(nls.model.quadratic) + qnorm(.025)*summary(nls.model.quadratic)$parameters[,"Std. Error"],
	coef(nls.model.quadratic) - qnorm(.025)*summary(nls.model.quadratic)$parameters[,"Std. Error"]
)
