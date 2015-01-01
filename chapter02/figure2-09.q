############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-09.q                                             #
#                                                          #
############################################################
 
par(mfrow=c(2,1), xaxs="i", yaxs="e", las=1, lwd=3)

# upper plot
x <- seq(-4, 4, 0.1)
plot(x, dnorm(x), type="l", xlab="", 
	ylab="", ylim=c(0,0.5))

# note Ruppert wants a t distribution with 
# 5 df's, mean 0 and sd 1, so scaling is needed
# variance of a standard t distribution is (v / (v-2) )
# where v = deg. freedom
#
sc <- sqrt(5/3)
lines(x, sc*dt(sc*x, df=5), lty=8)
abline(v=c(-2,-1,1,2),lwd=1)
mtext("left tail", line=1, side=3, at=-3, adj=0.5)
mtext("left shoulder", line=1, side=3, at=-2, adj=0)
mtext("center", line=1, side=3, at=0, adj=0.5)
mtext("right shoulder", line=1, side=3, at=1, adj=0)
mtext("right tail", line=1, side=3, at=3, adj=0.5)
text(-0.75, 0.2, "normal\ndistribution", adj=0)
text(1.9, 0.4, "t-distribution", adj=0)
arrows(-0.25, 0.25, 0.25, dnorm(0.25), size=.1)
arrows(1.75, 0.4, 0.5, sc*dt(0.5*sc,df=5), size=.1)

# bottom plot
x <- x[x >= 2.4]
plot(x, dnorm(x), type="l", xlab="", ylab="")
lines(x, sc*dt(sc*x, df=5), lty=8)
text(2.45, 0.003, "normal\ndistribution", adj=0)
text(3.3, 0.012, "t-distribution", adj=0)
arrows(2.5, 0.006, 2.7, dnorm(2.7), size=.1)
arrows(3.3, 0.012, 3.1, sc*dt(3.1*sc,df=5), size=.1)



par(mfrow=c(1,1))
