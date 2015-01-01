############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure7-2.q                                              #
#                                                          #
############################################################

par(lwd=3, xaxs="i", yaxs="i",las=1)
n <- seq(0,1.5,0.05)
xseq <- seq(0,1.5,0.5)
yseq <- seq(0,1.4,0.2)
plot(n,n,type="n",xlab="beta",ylab="risk premium",axes=F)
abline(c(0,0.8))
abline(v=1,lty=2)
points(0.6,0.3,pch="+")
text(0.65,0.25,"J",adj=0,cex=2)
text(0.3,0.1,"non-\naggressive",adj=0)
text(1.1,0.1,"aggressive",adj=0)
text(1.2,0.5,"Market\nportfolio",adj=0)
text(1.2,1.2,"SML",adj=0)
arrows(1.15,0.65,1.0,0.8)
arrows(1.23,1.1,1.2,0.8*1.2)
axis(1, at=xseq)
axis(2, at=yseq, adj=1)
box()
