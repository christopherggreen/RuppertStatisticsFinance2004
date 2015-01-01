############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure12-11.q                                            #
#                                                          #
############################################################

par(mfrow=c(2,2),las=1)
g <- function(epsilon, theta, gamma=1) { 
	theta*epsilon + gamma*(abs(epsilon) - sqrt(2/pi))
}
eps <- seq(-4,4,0.1)

plot( eps, g(eps, -0.7), type="l", lty=4, ylab="g(epsilon-t)", xlab="epsilon", main="theta=-0.7")
abline(h=0)
abline(v=0)
box()
plot( eps, g(eps,  0.0), type="l", lty=4, ylab="g(epsilon-t)", xlab="epsilon", main="theta=0.0")
abline(h=0)
abline(v=0)
box()
plot( eps, g(eps,  0.7), type="l", lty=4, ylab="g(epsilon-t)", xlab="epsilon", main="theta=-0.7")
abline(h=0)
abline(v=0)
box()
plot( eps, g(eps, -1.0), type="l", lty=4, ylab="g(epsilon-t)", xlab="epsilon", main="theta=-1.0")
abline(h=0)
abline(v=0)
box()

par(mfrow=c(1,1))
