############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure10-3.q                                             #
#                                                          #
############################################################

# 10.3
# run figure10-2 first to load the data
par(mfrow=c(5,2),xaxs="i",yaxs="i",las=1)
for ( country in 	countries.names ) {
	zz <- acf(countries.logreturns[,country],plot=F)
	plot(zz$lag[-1], zz$acf[-1,,], type="h", xlab="lag", 
		ylab="corr", main=country, ylim=c(-0.35,0.35),lwd=2,
		cex=0.6,mar=c(5,7,5,3))
	points(zz$lag[-1],zz$acf[-1,,],pch=16,cex=.6)
	abline(h=0)
	cl <- qnorm(0.5 + .95/2)/sqrt(zz$n.used)
	abline(h=c(-cl,cl),lty=3,lwd=2)
	grid.render(grids=list(x=seq(5,25,5),lty=2,lwd=1))	
}
par(mfrow=c(1,1))
