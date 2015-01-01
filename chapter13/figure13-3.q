############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure13-3.q                                             #
#                                                          #
############################################################

f.true <- function(x) 2*x*x*sin(4*x)

n      <- 15
sigma  <- .4
xlower <- 0
xupper <- 0.7

xseq <- seq(xlower, xupper, length=n)

real.data <- f.true(xseq)
fake.data <- real.data + rnorm(n, 0, sigma)

# linear regression
plot( xseq, fake.data, type="p", pch=16, xlab="", ylab="", las=1)
lines( xseq, real.data )
abline(lsfit(xseq,fake.data), lty=3)

# nonparametric regression using penalized splines
nonp.fit <- Pspline05(xseq, fake.data)
lines( xseq, nonp.fit$yhat, lty=5)
key(corner=c(1,0), border=T, 
	text=list(c("data","true curve","linear fit","nonp fit")),
	points=list(pch=c(16,0,0,0)),
	lines=list(lty=c(0,1,3,5)))
