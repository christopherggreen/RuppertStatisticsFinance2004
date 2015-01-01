###########################################################
#                                                         #
# Copyright (c) Christopher G. Green, 2006                #
#                                                         #
###########################################################
############################################################
#                                                          #
# pareto.q                                                 #
#                                                          #
############################################################

# pareto distribution functions

# PDF for pareto
dpareto <- function(x, a, cmin) { 
	ifelse(x > cmin, (a*(cmin^a)/(x^(a+1))), NA)
}

# CDF for pareto
ppareto <- function(x, a, cmin) {
	ifelse(x > cmin, 1 - (cmin/x)^a, NA)
}
