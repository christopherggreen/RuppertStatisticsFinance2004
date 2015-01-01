############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-12.q                                             #
#                                                          #
############################################################

# load default data
default.data <- read.table("../common/data/DefaultData.txt", 
	skip=4, header=F)
names(default.data) <- c("rating","frequency")
# frequencies are expressed as percentages
# for modeling we need them expressed as probabilities
default.data$frequency <- default.data$frequency/100
# don't forget to rescale for plots!


# nonlinear regression
b0.seq <- seq(-5,5,0.1)
b1.seq <- seq(-5,5,0.1)
m0 <- length(b0.seq)
m1 <- length(b1.seq)
# find a good starting value by calculating some sums of squares
# rows are observations, columns are candidate b0/b1 values
ss.b0.b1 <- colSums((matrix(rep(default.data$frequency, m0*m1), 
		nrow=nrow(default.data), ncol=m0*m1) - 
			exp(cbind(1,default.data$rating) %*% 
				rbind(rep(b0.seq,m1),rep(b1.seq,each=m0))) )^2)
# if you want to see them all
cbind(rep(b0.seq,m1), rep(b1.seq,each=m0), ss.b0.b1 )
# easier to find the minimum
cbind(rep(b0.seq,m1), rep(b1.seq,each=m0))[ss.b0.b1==min(ss.b0.b1),]

# from the above seems like (-5,-5) is a decent place to start
fit.nls <- nls( frequency ~ exp(b0 + b1*rating), 
	data=default.data, start=list(b0=-4.6,b1=0.1))
	
par(las=1,lwd=3,xaxs="i",yaxs="i")
plot(default.data$rating, 100*default.data$frequency, type="p", 
	xlab="rating", ylab="default probability",ylim=c(-4,14))
abline(h=0, lwd=1)
abline(lsfit(default.data$rating,100*default.data$frequency))
lines( default.data$rating, 100*predict(fit.nls), lty=3 )

key(corner=c(0,1), border=T, text=list(c("data","linear","exponential")),
	points=list(pch=c(1,0,0)), lines=list(lty=c(1,1,3)))


