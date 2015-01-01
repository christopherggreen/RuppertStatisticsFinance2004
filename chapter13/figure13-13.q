############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page414.q                                                #
#                                                          #
############################################################

diff.eu01 <- diff(eurodata$eu01)
rate.eu01 <- eurodata$eu01[-nrow(eurodata)]
rate2.eu01 <- rate.eu01^2
plus1 <- plus2 <- plus3 <- plus4 <- rep(0,length=length(rate.eu01))
plus1[rate.eu01 > 0.08] <- (rate.eu01[rate.eu01 > 0.08] - 0.08)^2
plus2[rate.eu01 > 0.12] <- (rate.eu01[rate.eu01 > 0.12] - 0.12)^2
plus3[rate.eu01 > 0.16] <- (rate.eu01[rate.eu01 > 0.16] - 0.16)^2
plus4[rate.eu01 > 0.20] <- (rate.eu01[rate.eu01 > 0.20] - 0.20)^2

# fit the best model
bestcp.model <- lm(  diff.eu01 ~ plus3 + plus4 )
bestcp.resid <- resid(bestcp.model)
bestcp.resid2 <- bestcp.resid^2

stepwise.model <- leaps( cbind(rate.eu01,rate2.eu01,plus1,plus2,plus3,plus4), bestcp.resid2 )

# order by cp
data.frame(stepwise.model[c("size","Cp","label")])[order(stepwise.model$Cp),]

bestcp.model.second <- lm( bestcp.resid2 ~ plus1 + plus2)
plot( rate.eu01, predict(bestcp.model.second), type="p", pch="+", 
	ylab="squared volatility", xlab="Euro rate")
