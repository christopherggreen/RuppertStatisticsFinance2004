############################################################
#                                                          #
# bondvalue.q                                              #
#                                                          #
############################################################

# direct translation of Ruppert's bondvalue.m code

bondvalue <- 
#	Computes bv = bond values (current prices) 
#
#		INPUT
#	cc = coupon payment (semi-annual)
#	TT = time to maturity (in years)
#	rr = yields to maturity (semi-annual rates)
#	parval = par value
#
#   cc, TT, rr, and parval can be scalars and/or vectors of the same length.
#
function (cc,TT,rr,parval) {
	cc/rr + (parval - cc/rr) * ((1+rr)^(-2*TT))
}
