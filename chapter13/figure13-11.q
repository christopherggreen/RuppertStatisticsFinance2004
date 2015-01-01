############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-11.q                                            #
#                                                          #
############################################################

# run figure13-1.q to load the data

diff.eu01 <- diff(eurodata$eu01)
rate.eu01 <- eurodata$eu01[-nrow(eurodata)]
plus1 <- plus2 <- plus3 <- plus4 <- rep(0,length=length(rate.eu01))
plus1[rate.eu01 > 0.08] <- rate.eu01[rate.eu01 > 0.08] - 0.08
plus2[rate.eu01 > 0.12] <- rate.eu01[rate.eu01 > 0.12] - 0.12
plus3[rate.eu01 > 0.16] <- rate.eu01[rate.eu01 > 0.16] - 0.16
plus4[rate.eu01 > 0.20] <- rate.eu01[rate.eu01 > 0.20] - 0.20

linspline.model <- lm( diff.eu01 ~ rate.eu01 + plus1 + plus2 + plus3 + plus4)
plot( rate.eu01, predict(linspline.model), type="p", pch=3, xlab="Euro rate", ylab="drift" )
