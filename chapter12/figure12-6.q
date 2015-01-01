############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure12-6.q                                             #
#                                                          #
############################################################

par(mfrow=c(3,1),las=1)

# generate white noise
epsilons <- rnorm(41000,0,1)
burnin <- 1:1000

# arch(1) process
a1 <- rep(NA,41000)
sigma1 <- rep(NA,41000)

for ( i in 1:41000 ) {
	sigma1[i] <- if (i == 1) 1 else sqrt(1 + 0.9*(a1[i-1]^2))
	a1[i] <- sigma1[i] * epsilons[i]
}
plot(a1[-burnin], type="l", main="alpha_1 = 0.9)", xlab="", ylab="")

# arch(1) process
a2 <- rep(NA,41000)
sigma2 <- rep(NA,41000)

for ( i in 1:41000 ) {
	sigma2[i] <- if (i == 1) 1 else sqrt(1 + 1*(a2[i-1]^2))
	a2[i] <- sigma2[i] * epsilons[i]
}
plot(a2[-burnin], type="l", main="alpha_1 = 1)", xlab="", ylab="")

# arch(1) process
a3 <- rep(NA,41000)
sigma3 <- rep(NA,41000)

for ( i in 1:41000 ) {
	sigma3[i] <- if (i == 1) 1 else sqrt(1 + 1.8*(a3[i-1]^2))
	a3[i] <- sigma3[i] * epsilons[i]
}
plot(a3[-burnin], type="l", main="alpha_1 = 1.8)", xlab="", ylab="")


par(mfrow=c(1,1))
