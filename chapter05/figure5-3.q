############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure5-3.q                                              #
#                                                          #
############################################################

dw <- 0.01
w <- seq(0,1,dw)

mu1 <- 0.14
mu2 <- 0.09
sigma1 <- .20
sigma2 <- .15
rf.rate <- 0.03

exp.return <- mu2 + (mu1 - mu2)*w
xgrid <- seq(-0.05,0.250,0.050)
ygrid <- seq( 0.00,0.150,0.025)
 
par(lwd=3,mfrow=c(2,2), xaxs="e",yaxs="i", pty="s")

for ( rho in c(0.7,0.3,0,-0.7) ) {

	risk <- sqrt((sigma1^2) * (w^2) + 
		(sigma2^2) * ((1-w)^2) + 2*w*(1-w)*rho*sigma1*sigma2)
	
	plot(risk,exp.return, type="l", axes=F, 
		xlab="", ylab="", ylim=range(ygrid), xlim=range(xgrid))
	axis(1, at=xgrid)
	axis(2, at=ygrid, adj=1)
	box()
	mtext("sigma_R", side=1, line=2)
	mtext("E(R)", side=2, line=4)

	sr <- (exp.return - rf.rate)/risk
	tan.index <- which(sr==max(sr))
	if ( length(tan.index) > 1 )
		stop("Tangency portfolio should be unique!")
	typical.index <- gmv.index + 10

	text(0, rf.rate, "F", cex=1.5)
	text(risk[tan.index]-1.5*dw, exp.return[tan.index], "T", cex=1.5)

	segments(0+dw,rf.rate,risk[tan.index],exp.return[tan.index],	lty=4, lwd=3)
	
	text(0.00, 0.13, paste("rho = ",rho,"\n"), adj=0)
		
}
