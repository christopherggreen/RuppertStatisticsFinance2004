############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure5-5.q                                              #
#                                                          #
############################################################

# run pages150-151.q first

omegaP <- t(
	matrix(rep(bg[,1],50),nrow=3,ncol=50) +
	matrix(rep(seq(min(bmu), max(bmu), length=50),3),byrow=T, nrow=3) * 
	matrix(rep(bh[,1],50),nrow=3,ncol=50)
)

par(lwd=3, xaxs="e", yaxs="e")
xgrid=seq( 0.045, 0.08, 0.005)
ygrid=seq(-0.4,1,0.2)
plot(muP, pmax(omegaP[,1],omegaP[,2],omegaP[,3]), type="n", 
	axes=F, xlab="", ylab="",
	xlim=range(xgrid), ylim=range(ygrid))
lines(muP[ind], omegaP[ind,1], lty=1)
lines(muP[ind], omegaP[ind,2], lty=2)
lines(muP[ind], omegaP[ind,3], lty=3)

mtext("muP", side=1, line=2)
mtext("weight", side=2, line=4)
grid.render(grid=list(x=xgrid,y=ygrid,lwd=1,lty=2))
axis(1, at=xgrid)
axis(2, at=ygrid, adj=1)
box()

key(0.073, -0.05, corner=c(0,0), border=T, lines=list(lty=c(1,2,3)), 
	text=list(c("w1","w2","w3")))
