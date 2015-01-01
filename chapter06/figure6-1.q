############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-1.q                                              #
#                                                          #
############################################################

# make some data
dfx <- runif(8)
dfy <- 0.1 + 0.95*dfx + rnorm(8,mean=0,sd=0.1)
fit <- lm(dfy ~ dfx)
dfyhat <- predict(fit, data.frame(dfx), type="response")
par(xaxs="i",yaxs="i",lwd=3,las=1)
plot(dfx, dfy, type="p", pch=3, cex=1.5, xlab="", ylab="",
	ylim=range(c(dfy,dfyhat))+c(-0.1,0.1),xlim=c(0,1))
abline(coef(fit))
points(dfx, dfyhat, pch=1, cex=1.5)
segments(dfx, dfy, dfx, dfyhat)
text(locator(1), "Fitted value", cex=2)
text(locator(1), "residual", cex=2)
text(locator(1), "Y", cex=2)
