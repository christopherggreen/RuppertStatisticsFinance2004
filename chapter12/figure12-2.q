############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure12-2.q                                             #
#                                                          #
############################################################

par(mfrow=c(2,2), xaxs="i", yaxs="i",las=1)
# generate white noise
epsilons <- rnorm(70,0,1)
burnin <- 1:10
# white noise
plot(epsilons[-burnin], type="l", main="white noise", xlab="", ylab="")

# arch(1) process
a <- rep(NA,70)
sigma <- rep(NA,70)

for ( i in 1:70 ) {
	sigma[i] <- if (i == 1) 1 else sqrt(1 + 0.95*(a[i-1]^2))
	a[i] <- sigma[i] * epsilons[i]
}
plot(sigma[-burnin], type="l", main="conditional std. dev.", xlab="", ylab="")
plot(a[-burnin], type="l", main="ARCH(1)", xlab="", ylab="")

# ar(1)/arch(1)
u <- rep(NA,70)
for ( i in 1:70 ) {
	u[i] <- 0.1 + if (i > 1) 0.8*(u[i-1] - 0.1) + a[i] else 0
}	
plot(u[-burnin], type="l", main="AR(1)/ARCH(1)", xlab="", ylab="")
par(mfrow=c(1,1))
