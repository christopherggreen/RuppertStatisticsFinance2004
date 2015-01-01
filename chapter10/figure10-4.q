############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure10-4.q                                             #
#                                                          #
############################################################

# 10.4
# run figure10-2.q first to load the data
#
par(mfrow=c(5,2),xaxs="i",yaxs="i",las=1)
for ( country in 	countries.names ) {
	qqplot.matlab(countries.logreturns[,country], 
		 xlabels=seq(-0.4,0.4,0.05),
		 qlabels=c(0.003,0.01,0.02,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.98,0.99,0.997),
		 xlab="",plot.title=country,cex=1,lwd=1)
}
par(mfrow=c(1,1))
