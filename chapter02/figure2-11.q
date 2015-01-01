############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-11.q                                             #
#                                                          #
############################################################
 
x <- seq(-10,15,0.1)
# normal
nx <- dnorm(x,0,sqrt(3.4))
# mixture of normals
mx <- 0.9*dnorm(x) + 0.1*dnorm(x,0,5)

par(mfrow=c(2,2), lwd=3, las=1, xaxs="i", yaxs="i")

plot(x, nx, xlim=c(-5,12), ylim=c(0,0.4),
	type="l", xlab="", ylab="")
lines(x, mx, lty=3)
title("densities")
key(11.9, 0.39, border=T, corner=c(1,1), 
	text=list(c("normal","normal mix")),
	lines=list(lty=c(1,3)))

ind <- x >= 4
x   <- x[ind]
nx  <- nx[ind]
mx  <- mx[ind]
plot(x, nx, xlim=c(4,12), ylim=c(0,0.025),
	type="l", xlab="", ylab="")
lines(x, mx, lty=3)
title("densities---detail")
key(11.9, 0.024, border=T, corner=c(1,1), 
	text=list(c("normal","normal mix")),
	lines=list(lty=c(1,3)))
	
qqplot.matlab(rnorm(200,0,sqrt(3.4)),
	xlabels=seq(-2, 2, 1),
	qlabels=c(0.003, 0.01, 0.02, 0.05, 0.10, 0.25, 0.50, 0.75, 
	0.90, 0.95, 0.98, 0.99, 0.997),
	plot.title="normal plot---normal"
)

qqplot.matlab(rnorm(200,0,c(1,5)[rbinom(200, size=2, p=0.9)]),
	xlabels=seq(-10, 15, 5),
	qlabels=c(0.003, 0.01, 0.02, 0.05, 0.10, 0.25, 0.50, 0.75, 
	0.90, 0.95, 0.98, 0.99, 0.997),
	plot.title="normal plot---normal mix"
)

par(mfrow=c(1,1))
