############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page375.q                                                #
#                                                          #
############################################################

# run figure12.5 first to load the data
# S+FinMetrics is needed for the garch command
sp500ret.garch.model <- garch( formula.mean=~pr105$GPW[-1]+pr105$DR3[-1]+ar(1),
						formula.var=~garch(1,1), 
							series=pr105$RETURNSP[-1] )
sp500ret.garch.model
summary(sp500ret.garch.model)
