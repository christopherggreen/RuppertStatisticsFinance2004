############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page385.q                                                #
#                                                          #
############################################################

# there's not much hope of us reproducing table 12.2 since SAS and
# S-Plus compute AIC slightly differently, and we don't have the
# exact algorithm

# here is the fit of the ar(1)/e-garch(1,1)-m

sp500ret.egarchm.model <- garch( formula.mean=~pr105$GPW[-1]+pr105$DR3[-1]+ar(1)+sd.in.mean,
						formula.var=~egarch(1,1), 
							series=pr105$RETURNSP[-1] )
sp500ret.egarchm.model
summary(sp500ret.egarchm.model)

