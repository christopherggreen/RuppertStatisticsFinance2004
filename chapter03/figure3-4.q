############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure3-4.q                                              #
#                                                          #
############################################################

# load ge data
price <- read.table("../common/data/ge.dat",skip=1)[[1]]
logreturn <- diff(log(price))

# statistics a la proc univariate
summary(logreturn)
var(logreturn)
skewness(logreturn)
kurtosis(logreturn)

shapiro.test(logreturn)
ks.gof(logreturn)

par(mfrow=c(3,2), xaxs="i",yaxs="i",lwd=3, pty="s", mar=c(3,1,2,1)+.1,las=1)

plot(price, xlab="",ylab="price", type="l", axes=F, ylim=c(40,60))
title("GE, daily - 12/17/99 to 12/15/00")
axis(1, at=seq(0,250,50))
axis(2, at=seq(40,60,5), adj=1)
box()

plot(diff(price)/price[-length(price)], xlab="",
	ylab="return", type="l", axes=F)
axis(1, at=seq(0,250,50))
axis(2, at=seq(-0.06,0.06,0.02), adj=1)
box()
	
plot(logreturn, xlab="", ylab="log return", type="l", axes=F)
axis(1, at=seq(0,250,50))
axis(2, at=seq(-0.06,0.06,0.02), adj=1)
box()

qqplot.matlab(logreturn, 
	xlabels=c(-0.04, -0.02, 0, 0.02, 0.04),
	qlabels=c(0.001,0.003,0.01,0.02,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.98,0.99,0.997,0.999),
	plot.title="Normal plot of log returns",
	xlab="log.return"
)


plot(abs(logreturn), xlab="", ylab="volatility", type="p", pch=".",
	axes=F, xlim=c(0,250))
lines(lowess(abs(logreturn)), lwd=4)
axis(1, at=seq(0,250,50))
axis(2, at=seq(0,0.06,0.02), adj=1)
box()
par(mfrow=c(1,1))
