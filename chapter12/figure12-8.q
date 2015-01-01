############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure12-8.q                                             #
#                                                          #
############################################################

par(mfcol=c(2,4),las=1)

# generate white noise
epsilons <- rnorm(2100,0,1)
burnin <- 1:100

sigma <- rep(NA, 2100)
a <- rep(NA, 2100)

for ( i in 1:2100 ) {
	sigma[i] <- if (i == 1) 1 else sqrt(1 + 0.95*(a[i-1]^2) + 0.05*(sigma[i-1]^2))
	a[i] <- sigma[i] * epsilons[i]
}
plot(sigma[-burnin], type="l", main="alpha_1 = 0.95, cond std. dev.", xlab="", ylab="")
plot(a[-burnin], type="l", main="alpha_1 = 0.95, GARCH(1,1)", xlab="", ylab="")

sigma <- rep(NA, 2100)
a <- rep(NA, 2100)
for ( i in 1:2100 ) {
	sigma[i] <- if (i == 1) 1 else sqrt(1 + 0.4*(a[i-1]^2) + 0.6*(sigma[i-1]^2))
	a[i] <- sigma[i] * epsilons[i]
}
plot(sigma[-burnin], type="l", main="alpha_1 = 0.4, cond std. dev.", xlab="", ylab="")
plot(a[-burnin], type="l", main="alpha_1 = 0.4, GARCH(1,1)", xlab="", ylab="")

sigma <- rep(NA, 2100)
a <- rep(NA, 2100)
for ( i in 1:2100 ) {
	sigma[i] <- if (i == 1) 1 else sqrt(1 + 0.2*(a[i-1]^2) + 0.8*(sigma[i-1]^2))
	a[i] <- sigma[i] * epsilons[i]
}
plot(sigma[-burnin], type="l", main="alpha_1 = 0.2, cond std. dev.", xlab="", ylab="")
plot(a[-burnin], type="l", main="alpha_1 = 0.2, GARCH(1,1)", xlab="", ylab="")

sigma <- rep(NA, 2100)
a <- rep(NA, 2100)
for ( i in 1:2100 ) {
	sigma[i] <- if (i == 1) 1 else sqrt(1 + 0.02*(a[i-1]^2) + 0.98*(sigma[i-1]^2))
	a[i] <- sigma[i] * epsilons[i]
}
plot(sigma[-burnin], type="l", main="alpha_1 = 0.02, cond std. dev.", xlab="", ylab="")
plot(a[-burnin], type="l", main="alpha_1 = 0.02, GARCH(1,1)", xlab="", ylab="")

par(mfcol=c(1,1))
