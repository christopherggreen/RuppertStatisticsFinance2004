############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure5-2.q                                              #
#                                                          #
############################################################

# increment in w
dw <- 0.01
# weight sequence
w <- seq(0,1,dw)

# means and std. dev's of the two assets
mu1 <- 0.14
mu2 <- 0.08
sigma1 <- .20
sigma2 <- .15

# correlation
rho <- 0

# risk-free rate
rf.rate <- 0.06

# return and risk
exp.return <- mu2 + (mu1 - mu2)*w
risk <- sqrt((sigma1^2) * (w^2) + (sigma2^2) * ((1-w)^2) + 2*w*(1-w)*rho*sigma1*sigma2)

# coordinates of x and y grids	
xgrid <- seq(-0.05,0.300,0.050)
ygrid <- seq( 0.05,0.150,0.025) 

# set up lines, axes, etc.
par(lwd=3,xaxs="e",yaxs="i", pty="s", las=1)

plot(risk, exp.return, type="l", axes=F, 
	xlab="", ylab="", ylim=range(ygrid), xlim=range(xgrid))
	
# mimic axes in the book
axis(1, at=xgrid)
axis(2, at=ygrid, adj=1)
box()
mtext("\"Risk\" = sigma_R", side=1, line=2)
mtext("\"Reward\" = E(R)", side=2, line=4)

# add a grid
grid.render(grids=list(x=xgrid,y=ygrid,lty=2,lwd=1))

# add global minimum variance and tangency portfolios
gmv.index <- which(risk==min(risk))
if ( length(gmv.index) > 1 ) 
	stop("Global minimum variance portfolio should be unique!")
sr <- (exp.return - rf.rate)/risk
tan.index <- which(sr==max(sr))
if ( length(tan.index) > 1 )
	stop("Tangency portfolio should be unique!")
typical.index <- gmv.index + 10

# annotations
text(risk[1]+1.5*dw, exp.return[1], "R2", cex=1.5)
text(risk[length(w)]+1.5*dw, exp.return[length(w)], "R1", cex=1.5)
text(0, rf.rate, "F", cex=1.5)
text(risk[tan.index]-1.5*dw, exp.return[tan.index], "T", cex=1.5)

points(risk[c(gmv.index,typical.index,tan.index)], 
	exp.return[c(gmv.index,typical.index,tan.index)],
	pch=11, cex=1)
	
segments(0+dw,rf.rate,risk[1],exp.return[1],lty=2, lwd=3)
segments(0+dw,rf.rate,risk[gmv.index]  ,exp.return[gmv.index]  ,lty=3, lwd=3)
segments(0+dw,rf.rate,risk[typical.index],exp.return[typical.index],	lty=4, lwd=3)

text(0.02,0.14,
	"Efficient frontier", adj=0)
text(0.16,exp.return[gmv.index]-1.5*dw,
	"minimum variance portfolio", adj=0)
text(0.17,exp.return[typical.index]+0.5*dw,
	"typical portfolio", adj=0)
	
arrows(0.165,exp.return[typical.index]+0.5*dw,
	risk[typical.index]+dw,exp.return[typical.index])
arrows(0.155,exp.return[gmv.index]-1.5*dw,
	risk[gmv.index],exp.return[gmv.index])
arrows(0.135,0.14,risk[tan.index+8],
	exp.return[tan.index+8])	
