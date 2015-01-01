############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure6-3.q                                              #
#                                                          #
############################################################

# run page190.q first

# figure 6.3
plot( bondprices$maturity, bondprices$price, pch="x", 
	type="p", xlab="maturity", ylab="price" )
lines( bondprices$maturity, predict(fit) )
key(corner=c(1,1), border=T, points=list(pch=c("x","-")),
	text=list(c("price","predicted price")) )