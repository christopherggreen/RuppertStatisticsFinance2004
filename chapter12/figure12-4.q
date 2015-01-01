############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure12-4.q                                             #
#                                                          #
############################################################

par(mfrow=c(3,2), xaxs="i", yaxs="i",las=1)
# generate white noise
epsilons <- rnorm(700,0,1)
burnin <- 1:100
# white noise
plot(epsilons[-burnin], type="l", main="white noise", xlab="", ylab="")

# garch(1,1) process
a <- rep(NA,700)
sigma <- rep(NA,700)

for ( i in 1:700 ) {
	sigma[i] <- if (i == 1) 1 else sqrt(1 + 0.08*(a[i-1]^2) + 0.9*(sigma[i-1]^2))
	a[i] <- sigma[i] * epsilons[i]
}
plot(sigma[-burnin], type="l", main="conditional std. dev.", xlab="", ylab="")
plot(a[-burnin], type="l", main="GARCH(1,1)", xlab="", ylab="")

# ar(1)/garch(1,1)
u <- rep(NA,700)
for ( i in 1:700 ) {
	u[i] <- 0.1 + if (i > 1) 0.8*(u[i-1] - 0.1) + a[i] else 0
}	
plot(u[-burnin], type="l", main="AR(1)/GARCH(1,1)", xlab="", ylab="")

# final plot is qq of garch(1,1)
qqplot.matlab(a[-burnin], seq(-20,25,5), 
	c(0.001, 0.003, 0.01, 0.02, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.98, 0.99, 0.997, 0.999), 
	plot.title="normal plot of GARCH(1,1)", pch="+")

par(mfrow=c(1,1))
