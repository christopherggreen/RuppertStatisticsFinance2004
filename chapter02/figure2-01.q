############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-01.q                                             #
#                                                          #
############################################################
 
# plotting parameters
par(mfrow=c(2,2), xaxs="e", yaxs="e", lwd=3, las=1)

# Ruppert did not give any particular information on distributions/parameters,
# so I have just used distributions that looked "close" to his graphs

# top left plot
pseq <- seq(0, 1, 0.01)
cdf1 <- function(x) {
	pbeta(x, shape1=1.6, shape2=1)
}
plot(pseq, cdf1(pseq), type="l", axes=F, xlab="", ylab="")

# annotations
qq <- 0.5
xx <- qbeta(qq, shape=1.6, shape2=1)
segments(xx  ,0 ,xx,qq,lty=8)
segments(0   ,qq,xx,qq,lty=8)
axis(1, at=c(0,0.5,1), las=1)
axis(2, at=c(0,0.2,0.4,0.6,0.8,1), las=1)
box()
text(0.1, qq+0.1, "q", cex=1.5, adj=0)
arrows(0.1, qq+0.1, 0, qq, size=0.1)
text(xx+0.1, 0.1, "F^{-1}(q)", cex=1.5, adj=0)
arrows(xx+0.1, 0.1, xx, 0, size=0.1)

# top right plot
cdf2 <- function(x) {
	y <- x
	y[           x < 0.2] <- 0
	y[0.2 <= x & x < 0.6] <- 0.2
	y[0.6 <= x & x < 0.8] <- 0.6
	y[	0.8 <= x & x < 0.9] <- 0.9
	y[0.9 <= x          ] <- 1
	
	y
}
plot(pseq, cdf2(pseq), type="s", axes=F, xlab="", ylab="")
xx <- 0.6
qq <- 0.5
segments(0.0,qq ,xx ,qq , lty=8)
segments(xx ,0.0,xx ,0.2, lty=8)
axis(1, at=c(0,0.5,1), las=2)
axis(2, at=c(0,0.2,0.4,0.6,0.8,1), las=2)
box()
text(0.1, qq+0.1, "q", cex=1.5, adj=0)
arrows(0.1, qq+0.1, 0, qq, size=0.1)
text(xx+0.1, 0.1, "F^{-1}(q)", cex=1.5, adj=0)
arrows(xx+0.1, 0.1, xx, 0, size=0.1)

# bottom left plot
cdf3 <- function(x) {
	y <- x
	# the next step is implied. don't actually need to run it
	# as it is covered in
	# the statement above
	# y[           x < 0.5] <- x[ x < 0.5 ]
	# 
	y[0.5  <= x & x < 0.75] <- 0.5
	y[0.75 <= x & x < 1.00] <- 2 * x[0.75 <= x & x < 1.00] - 1
	
	y
}
plot(pseq, cdf3(pseq), type="l", axes=F, xlab="", ylab="")
xx <- 0.75
qq <- 0.50
segments(0.00,qq  ,qq  ,qq  , lty=8)
segments(qq  ,0.00,qq  ,qq  , lty=8)
segments(xx  ,0.00,xx  ,qq  , lty=8)
axis(1, at=c(0,0.5,1), las=2)
axis(2, at=c(0,0.2,0.4,0.6,0.8,1), las=2)
box()
text(0.1, qq+0.1, "q", cex=1.5, adj=0)
arrows(0.1, qq+0.1, 0, qq, size=0.1)
text(xx+0.1, 0.1, "F^{-1}(q)", cex=1.5, adj=0)
arrows(xx+0.1, 0.1, xx, 0, size=0.1)
arrows(xx+0.1, 0.1, qq, 0, size=0.1)
arrows(xx+0.1, 0.1, (xx+qq)/2, 0, size=0.1)

# restore normal plot layout
par(mfrow=c(1,1))
