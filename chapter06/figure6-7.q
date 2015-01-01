############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-7.q                                              #
#                                                          #
############################################################

# ruppert doesn't give the details of how he simulated the data
# so we'll just do something that looks "similar"

x1 <- runif(100,0,1)
x2 <- runif(100,0,1)

# notice there are no negative values of y in his simulated data

y  <- 0.003 + 3.2*exp(1.4*x1) + 1.56*x2 + rnorm(100, 0, 2*x1)
summary(y)

par(mfrow=c(2,1), las=1, lwd=3)
plot(x1,y,ylim=c(0,30))
plot(x2,y,ylim=c(0,30))
par(mfrow=c(1,1))

