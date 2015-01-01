############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# qqplot.matlab.q                                          #
#                                                          #
############################################################

"qqplot.matlab" <- 
#
# makes the Q-Q plots found in Ruppert.
# S-Plus's qqnorm function works slightly differently than 
# MATLAB's, and there are enough Q-Q plots in the book to 
# warrant a function.
#
function(x, xlabels, qlabels, plot.title="Normal Probability Plot", 
	xlab="Data", ylab="Probability", ...) {
	
	par(xaxs="e",yaxs="e",lwd=3)
	# matlab puts the data on the x-axis, which
	# is the opposite of s-plus and r
	# use datax=T to make the plot the matlab way
	qqdata <- qqnorm(x, datax=T, pch="+",  
		xlab="", ylab="", axes=F, ...)
		
	# have to hack qqline though
	qa <- quantile(x, c(0.25, 0.75), na.rm=T)
	qb <- qnorm(c(0.25, 0.75))
	b <- (qb[2]-qb[1])/(qa[2]-qa[1])
	a <- qb[1] - qa[1]*b
	abline(a, b, lty=3)
	
	axis(1, at=xlabels, las=1)
	axis(2, adj=1, at=qnorm(qlabels), labels=qlabels, las=1)
	grid.render(grids=list(x=xlabels,y=qnorm(qlabels), 
		lty=2, lwd=1))
	box()
	title(plot.title)
	mtext(side=1,xlab, line=3)
	mtext(side=2,ylab, line=4)
	invisible(NULL)	
}
