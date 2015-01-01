############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-08.q                                             #
#                                                          #
############################################################
 
x <- 0:10

par(mfrow=c(2,2), xaxs="e", yaxs="e", lwd=3, las=1)
y <- dbinom(x, 10, .9)
barplot(y, xlab="x", ylab="P(X=x)", names=as.character(x))
box()
text(0, 0.35, paste("p = 0.9, S = ",round(skewness(y),4),"\nK = ", 
	round(kurtosis(y),4),sep=""), adj=0)

y <- dbinom(x, 10, .5)
barplot(y, xlab="x", ylab="P(X=x)", names=as.character(x))
box()
text(0, 0.2, paste("p = 0.5, S = ",round(skewness(y),4),"\nK = ", 
	round(kurtosis(y),4),sep=""), adj=0)
	
y <- dbinom(x, 10, .2)
barplot(y, xlab="x", ylab="P(X=x)", names=as.character(x))
box()
text(4, 0.3, paste("p = 0.2, S = ",round(skewness(y),4),"\nK = ", 
	round(kurtosis(y),4),sep=""), adj=0)
	
y <- dbinom(x, 10, .02)
barplot(y, xlab="x", ylab="P(X=x)", names=as.character(x))
box()
text(3, 0.4, paste("p = 0.02, S = ",round(skewness(y),4),"\nK = ", 
	round(kurtosis(y),4),sep=""), adj=0)


par(mfrow=c(1,1))
