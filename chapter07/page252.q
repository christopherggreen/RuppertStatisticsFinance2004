###########################################################
#                                                         #
# Copyright (c) Christopher G. Green, 2006                #
#                                                         #
###########################################################
############################################################
#                                                          #
# page252.q                                                #
#                                                          #
############################################################

# run page247.q first
capm.logret.monthly$time <- (seq(nrow(capm.logret.monthly)) - 30.5)/29.5

# the : means to fit the interaction between two variables
# equivalent to what Ruppert did in SAS
#
model.4 <- lm( microsoft.logret ~ sp500.logret + sp500.logret : time - 1,
	data=capm.logret.monthly, na.action=na.omit)
summary.proc.reg(model.4)
