############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure4-7.q                                              #
#                                                          #
############################################################

# data set not on ruppert's web site
# comparable data from the fed's statistical releases
tbill <- read.table("../common/data/FRB_H15.csv", sep=",", skip=7, header=F, 
	row.names=NULL, col.names=c("Date","Rate"))
	
par(mfrow=c(2,2), xaxs="i", yaxs="i", lwd=1)
plot(tbill$Rate, type="l", xlab="month since Dec. 1950", 
	ylab="interest rate", main="3 month T-bills", xlim=c(0,600),
	ylim=c(0,20))
plot(diff(tbill$Rate), type="l", xlab="month since Dec. 1950",
	ylab="1st difference", main="differences", xlim=c(0,600),
	ylim=c(-6,4))
acf.plot(acf(tbill$Rate, plot=F, lag.max=60), main="SACF")
acf.plot(acf(diff(tbill$Rate),plot=F), main="SACF of differences")
par(mfrow=c(1,1))
