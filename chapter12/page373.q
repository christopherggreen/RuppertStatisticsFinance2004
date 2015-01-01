############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page373.q                                                #
#                                                          #
############################################################

# S+FinMetrics is needed for garch tools
# load data
y <- read.table("../common/data/garch02.dat", skip=2, na.string=".")[[1]]
y.garch.model <- garch( formula.mean=~ar(1), formula.var=~garch(0,1), series=na.omit(y) )
y.garch.model
summary(y.garch.model)
