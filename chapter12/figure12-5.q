############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure12-5.q                                             #
#                                                          #
############################################################

# load data
pr105 <- read.table("../common/data/ex105.csv", sep=",", header=T,row.names=NULL,as.is=T)
pr105$DATE <- timeDate(pr105$DATE, in.format="%3m-%02Y", format="%02m/%02d/%Y")
pr105$DR3 <- c(NA,diff(pr105$R3))

# fit a factor model without any garch terms
fit.factor.model <- lm( RETURNSP ~ DR3 + GPW, data=pr105[-1,])

# plot of residuals over time
plot(pr105$DATE[-1], resid(fit.factor.model), 
	las=1, type="l", xlab="year", ylab="return residual")
