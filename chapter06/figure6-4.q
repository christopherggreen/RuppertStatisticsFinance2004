############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-4.q                                              #
#                                                          #
############################################################


n <- 11
# leverage point at 75
x1 <- c(sort(runif(n-1,0,10)), 75)
# leverage point is not an outlier
y1 <- 0.5*x1 + rnorm(n,0,0.1)
y2 <- y1
# creates an outlier that is a leverage point
y2[n] <- 10

fit1 <- lsfit(x1,y1)
fit2 <- lsfit(x1,y2)

ylim=range(y1,y2)
par(mfrow=c(2,1), las=1, lwd=3, cex=2)
plot(x1,y1,xlab="x",ylab="y",ylim=ylim,pch="+")
abline(fit1)
plot(x1,y2,xlab="x",ylab="y",ylim=ylim,pch="+")
abline(fit2)
par(mfrow=c(1,1))