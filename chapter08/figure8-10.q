############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure8-10.q                                             #
#                                                          #
############################################################

# run page283.q first
# run figure8-9.q first
# figure 8.10


# create a data frame containing values at which we want to 
# predict from the model
const.k.frame <- data.frame( 
	outer(rep(0,31),c(1,2),FUN=function(x,y) x^y),
	outer(seq(0,150,5)-63.040,c(1,2,3),FUN=function(x,y) x^y)
)
names(const.k.frame) <- c("K","K2","T.exp","T2","T3")
par(lwd=2,xaxs="i",yaxs="i",las=1)
plot(const.k.frame$T.exp+63.040, predict(impvol.model, const.k.frame), 
	type="l", xlab="time to maturity",ylab="implied volatility",
	xlim=c(0,150),ylim=c(0.019,0.028))
	
		
