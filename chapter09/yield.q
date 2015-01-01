############################################################
#                                                          #
# yield.q                                                  #
#                                                          #
############################################################

# direct translation of Ruppert's yield.m code

#	Computes the yield to maturity of a bond paying semi-annual
#	coupon payments
#
#	price, coupon payment, and time to maturity (in years) are set below
#
#	Use the function "bondvalue"
#
price    <- 1200	#	current price of the bond
CC       <- 40		#	coupon payment
TT       <- 30 	    #	time to maturity
parval   <- 1000	#	par value of the bond

lower <- .02 
upper <- .05
rr    <- seq(lower,upper,length=300)	# grid of values of yield to maturity
value <- bondvalue(CC,TT,rr,parval) 	# prices corresponding to r values

#	Find yield to maturity by interpolation
yield2M <- approx(x=spline(value,rr),xout=price)$y

#	Plot of price versus yield to maturity (Figure 9.1)
#	(not actually needed to find yield to maturity)
#
par(lwd=3,xaxs="i",yaxs="i")
plot(rr,value, type="l", xlab="yield to maturity",
	ylab="price of bond", main=paste("par=",parval,"coupon payment=",CC,"TT=",TT),
	xlim=c(lower,upper),ylim=range(value))
abline(v=yield2M)
abline(h=price)

cat("yield to maturity <- ",yield2M,"\n")
