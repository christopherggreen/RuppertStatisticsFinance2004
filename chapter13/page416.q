############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page416.q                                                #
#                                                          #
############################################################

diff.eu01 <- diff(eurodata$eu01)
rate.eu01 <- eurodata$eu01[-nrow(eurodata)]
rate2.eu01 <- rate.eu01^2
lagdiff.eu01 <- diff.eu01[-length(diff.eu01)]
lagdiff2.eu01 <- lagdiff.eu01^2

plus1 <- plus2 <- plus3 <- plus4 <- rep(0,length=length(rate.eu01))
plus1[rate.eu01 > 0.08] <- (rate.eu01[rate.eu01 > 0.08] - 0.08)^2
plus2[rate.eu01 > 0.12] <- (rate.eu01[rate.eu01 > 0.12] - 0.12)^2
plus3[rate.eu01 > 0.16] <- (rate.eu01[rate.eu01 > 0.16] - 0.16)^2
plus4[rate.eu01 > 0.20] <- (rate.eu01[rate.eu01 > 0.20] - 0.20)^2

plusLD1 <- rep(0, length=length(lagdiff.eu01))
plusLD1[lagdiff.eu01 > 0] <- lagdiff2.eu01[lagdiff.eu01 > 0]

# find the best model
# note that lagdiff.eu01 and lagdiff2.eu01 have length n-2, so 
# have to adjust other var's by dropping first observation
stepwise.model <- leaps( cbind(
	cbind(rate.eu01,rate2.eu01,plus1,plus2,plus3,plus4)[-1,],
	cbind(lagdiff.eu01,lagdiff2.eu01,plusLD1)), diff.eu01[-1] )

# order by cp
data.frame(stepwise.model[c("size","Cp","label")])[order(stepwise.model$Cp),]

# fit 2nd best model
bestcp.model.second <- lm( diff.eu01[-1] ~ plus4[-1] + lagdiff.eu01 + plusLD1  )

# stats
bestcp.model.second
summary(bestcp.model.second)
summary.proc.reg(bestcp.model.second)

# estimates of mu1 and mu2
mu1 <- coef(bestcp.model.second)[2] * plus4[-1]
mu2 <- as.vector(cbind(lagdiff.eu01,plusLD1) %*% 
	coef(bestcp.model.second)[3:4])

# now run figure13-14.q for plots






