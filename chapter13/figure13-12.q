############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-12.q                                            #
#                                                          #
############################################################

# figure13-1.q loads the data 

diff.eu01 <- diff(eurodata$eu01)
rate.eu01 <- eurodata$eu01[-nrow(eurodata)]
rate2.eu01 <- rate.eu01^2

###############################################################
# Ruppert doesn't really state what his candidate knots were...
# we'll kinda guess, so our picture may look a little different
###############################################################

knots <- seq(0.04,0.16,length=20)

# matrix of "plus functions"
plusmat <- matrix(0, nrow=length(rate.eu01), ncol=length(knots))
for( i in seq(along=knots)) {
	ind <- which(rate.eu01 >= knots[i])
	plusmat[ind,i] <- (rate.eu01[ind] - knots[i])^2
}
dimnames(plusmat) <- list(NULL,paste("k",1:20,sep=""))

twoknot.model    <- lm( diff.eu01 ~ rate.eu01 + rate2.eu01 + plusmat[,8] + plusmat[,14] )
tenknot.model    <- lm( diff.eu01 ~ rate.eu01 + rate2.eu01 + plusmat[,c(1:10)] )
twentyknot.model <- lm( diff.eu01 ~ rate.eu01 + rate2.eu01 + plusmat )

# for plotting purposes points need to be in increasing order of x-value (rate.eu01)
ind <- order(rate.eu01)
plotdata.tenknot    <- cbind(rate.eu01, predict(tenknot.model))[ind,]
plotdata.twoknot    <- cbind(rate.eu01, predict(twoknot.model))[ind,]
plotdata.twentyknot <- cbind(rate.eu01, predict(twentyknot.model))[ind,]

# notice the jagged behavior near the right side in these plots, as the models try to fit 
# the extreme euro rates

par(lwd=3,las=1)
plot( plotdata.tenknot,	type="l", xlab="Euro rate", ylab="drift",
	ylim=range(plotdata.tenknot[,2],plotdata.twoknot[,2],plotdata.twentyknot[,2]))
lines( plotdata.twoknot, lty=3)
lines( plotdata.twentyknot, lty=4)
key(corner=c(0,0), border=T, lines=list(lty=c(1,3,4)), text=list(c("10 knots","2 knots","20 knots")))


