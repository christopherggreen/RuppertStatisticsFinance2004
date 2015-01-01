############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# page410.q                                                #
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

quadspline.model <- lm( diff.eu01 ~ rate.eu01 + rate2.eu01 + plus1 + plus2 + plus3 + plus4 )
plot( rate.eu01, predict(quadspline.model), type="p", pch=3, xlab="Euro rate", ylab="drift" )
