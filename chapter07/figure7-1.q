############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure7-1.q                                              #
#                                                          #
############################################################

mu.rf        <- 0.06
mu.market    <- 0.15
sigma.market <- 0.22

# define cml as a function
cml <- function(sigr, muf, mum, sigm) { 
	muf + (mum - muf) * (sigr/sigm)
}

xseq <- seq(0,0.25,0.05)
yseq <- seq(0,0.16,0.02)
n <- seq(0,0.25,length=100)
par(lwd=3, cex=1.5, xaxs="i", yaxs="i")
plot(n,n,type="n",xlab="",ylab="",axes=F,
	ylim=range(yseq),xlim=range(xseq))

# lots of annotation...
segments(x1=0,y1=mu.rf,x2=sigma.market,y2=mu.market)
segments(x1=0,y1=mu.market,x2=sigma.market,lty=3)
segments(x1=sigma.market,y1=0,y2=mu.market,lty=3)
zz <- cml(0.12,mu.rf,mu.market,sigma.market)
segments(x1=0,y1=zz,x2=0.12,lwd=5)
segments(x1=0,y1=mu.rf,y2=zz,lwd=5)
text(0.05,0.13,"risk",cex=1.2,adj=0)
text(0.03,0.09,"reward",cex=1.2,adj=0)
text(0.13,0.09,"an efficient portfolio",
	cex=1.2,adj=0)
text(0.13,0.14,"CML",cex=1.2,adj=1)
text(0.23,0.13,"M",cex=1.2,adj=0)
text(0.03,0.01,"F",cex=1.2,adj=0)
arrows(0.23,0.135,sigma.market,mu.market)
arrows(0.135,0.135,0.175,
	cml(0.175,mu.rf,mu.market,sigma.market))
arrows(0.05,0.13,0.04,zz)
arrows(0.028,0.09,0.001,0.08)
arrows(0.028,0.01,0.001,mu.rf)
arrows(0.14,0.095,0.12,zz)
axis(1, at=xseq)
axis(2, at=yseq, adj=1)
box()

