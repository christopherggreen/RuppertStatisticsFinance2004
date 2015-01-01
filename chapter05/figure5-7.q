############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure5-7.q                                              #
#                                                          #
############################################################

par(lwd=3, xaxs="e", yaxs="e",las=1)
xgrid=seq( 0.045, 0.08, 0.005)
ygrid=seq(0,1,0.5)
plot(muP, pmax(omegaP[1,],omegaP[2,],omegaP[3,]), type="n", 
	axes=F, xlab="", ylab="",
	xlim=range(xgrid), ylim=range(ygrid))
lines(muP[Ieff], omegaP[1,Ieff], lty=1)
lines(muP[Ieff], omegaP[2,Ieff], lty=2)
lines(muP[Ieff], omegaP[3,Ieff], lty=3)

mtext("muP", side=1, line=2)
mtext("weight", side=2, line=4)
grid.render(grid=list(x=xgrid,y=ygrid,lwd=1,lty=2))
axis(1, at=xgrid)
axis(2, at=ygrid, adj=1)
box()

key(0.046, 0.98, corner=c(0,1), border=T, lines=list(lty=c(1,2,3)), 
	text=list(c("w1","w2","w3")))
