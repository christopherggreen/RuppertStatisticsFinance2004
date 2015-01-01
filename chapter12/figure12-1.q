############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure12-1.q                                             #
#                                                          #
############################################################

# load data
irates <- read.table("../common/data/WeeklyInterest.dat", skip=14, na.string="0")
names(irates) <- c("month","day","year","ff","tb03","cm10","cm30","discount","prime","aaa","unknown")
aaa.dif <- abs(diff(irates$aaa))
# add 0.5% to zeros
aaa.dif[aaa.dif == 0.0] <- 0.005

par(xaxs="i", yaxs="i", lwd=3,las=1)
plot(log10(aaa.dif), type="p", pch=".", axes=F, xlab="year", 
		ylab="|change| + 1/2%", ylim=c(-3,0.1))
lines(lowess(log10(aaa.dif), f=1/15))
axis(1, at=seq(par()$usr[1],par()$usr[2],length=6), 
	labels=as.character(seq(1970,1995,5)))
axis(2, at=c(-2,-1,0), labels=c("1e-2","1e-1","1e0"), adj=1)
box()

