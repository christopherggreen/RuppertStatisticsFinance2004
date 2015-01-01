############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure11-4.q                                             #
#                                                          #
############################################################

alpha <- seq(0.001, 0.1, length=1000)
VaR   <- function(a) { 178*((0.1/a)^0.262) }

par(xaxs="i",yaxs="i",las=1)
plot(alpha, VaR(alpha), type="l", axes=F, ylim=c(100,800))
axis(1, at=c(0.01, 0.025, 0.05, 0.1))
axis(2, at=round(c(VaR(c(0.01, 0.025, 0.05)),600,800),1), adj=1)
box()
segments(x1=0.01 , y1=0, y2=VaR(0.01 ))
segments(x1=0.025, y1=0, y2=VaR(0.025))
segments(x1=0.05 , y1=0, y2=VaR(0.05 ))

segments(x1=0, x2=0.01 , y1=VaR(0.01 ))
segments(x1=0, x2=0.025, y1=VaR(0.025))
segments(x1=0, x2=0.05 , y1=VaR(0.05 ))

VaR.parametric <- function(a) { 
	-10000*(mean(sp500.logret.last1000) + qnorm(a)*stdev(sp500.logret.last1000))
}
lines( alpha, VaR.parametric(alpha), lty=3)
text(0.06,150,"Pareto tail", adj=0)
text(0.045,500,"parametric", adj=0)
arrows(0.06,150,0.055,VaR(0.055))
arrows(0.045,500,0.02,VaR.parametric(0.02))
