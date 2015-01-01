############################################################
#                                                          #
# quantileknots01.q                                        #
#                                                          #
############################################################

# direct translation of Ruppert's MATLAB code

quantileknots01 <- 
#
#	Create knots at sample quantiles. 
#	
#		INPUT (required)
#	x = independent variable.  (The knots are at sample quantiles of the
#	unique x values.)
#	nknots = number of knots
#
#	USAGE: knots = quantileknots01(x,nknots,) 
#
#
#	Last edit: 9/16/20
#
function(x,nknots) {
	x <- unique(x)
	loc <- length(x) *  seq(1,nknots) / (nknots+1)

	# on MATLAB 5 rounds up, i.e., round(2.5) = 3
	# whereas on S-Plus 5 rounds to the nearest even, i.e.,
	# round(2.5) = 2
	#
	# for each element of x, check last digit. If it's a 5, fudge it with
	# .001
	
	fives <- which(sapply(as.character(loc), 
		function(y) substring(y,nchar(y),nchar(y))) == "5")
	loc[fives] <- loc[fives]+.001
	sort(x)[round(loc)] 
}
