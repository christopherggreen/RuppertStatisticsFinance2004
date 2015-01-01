############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure7-3.q                                              #
#                                                          #
############################################################

# plot shows a CML with slope (.105-.078)/(.36 - .25)
cml.slope <- (.105-.078)/(.36 - .25)
# equation of this line is then (y-0.078) = cml.slope*(x-0.25)
cml.intercept <- 0.078 - 0.25*cml.slope
# but cml's intercept is the expected risk-free return

sigma.i      <- 0.370
sigma.market <- 0.375
mu.i         <- 0.072
mu.rf        <- cml.intercept
mu.market    <- cml.intercept + sigma.market*cml.slope

beta.i <- 0.6
cov.i.market <- beta.i*sigma.market*sigma.market

cat("risk-free return",mu.rf,"\n")
cat("market return",mu.market,"\n")
cat("ith risky asset return",mu.i,"\n")
cat("ith standard deviation",sigma.i,"\n")
cat("market standard deviation",sigma.market,"\n")

wi   <- seq(-0.25,1,0.01)
xseq <- seq(0.25,0.5,0.05)
yseq <- seq(0.06,0.12,0.01)

exp.return <- mu.market + wi * (mu.i - mu.market)
risk       <- sqrt((wi*sigma.i)^2 + ((1-wi)*sigma.market)^2 + 
	2*wi*(1-wi)*cov.i.market)
par(las=1,lwd=2,xaxs="i",yaxs="i")
plot(risk, exp.return, type="n",	xlab="sigma_p",ylab="mu_p", 
	xlim=range(xseq),ylim=range(yseq))
lines(risk, exp.return, lty=4, lwd=2)
port.tan <- c(sigma.market,mu.market)	
points(port.tan[1],port.tan[2],pch="+",cex=2)
segments(0,mu.rf,port.tan[1],port.tan[2],lty=4,lwd=2)	
text(0.4,0.095,"tangency portfolio",adj=0)
text(0.38,0.075,"portfolios of T and i",adj=0)
text(0.26,0.101,"CML")
arrows(0.26,0.1,0.27,cml.intercept+0.27*cml.slope)
arrows(0.39,0.095,port.tan[1],port.tan[2])
arrows(0.38,0.075,risk[100],exp.return[100])
# have to fake the other curve
mu.i <- 0.035
beta.i <- 0.25
cov.i.market <- beta.i*sigma.market*sigma.market
exp.return <- mu.market + wi * (mu.i - mu.market)
risk       <- sqrt((wi*sigma.i)^2 + ((1-wi)*sigma.market)^2 + 
	2*wi*(1-wi)*cov.i.market)
lines(risk, exp.return, lty=1, lwd=4)
