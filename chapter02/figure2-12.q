############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure2-12.q                                             #
#                                                          #
############################################################
 
x <- seq(0.26, 4, 0.01)

# look carefully at figure 2.12
# the ticks on the x-axis are 
# equidistant even though the 
# numbers they represent are not
#
# this function rescales the x values
# in the plot so that a plot with 
# a logarithmic (base e) x-axis looks
# like figure 2.12
g <- function(x) exp(logb(x,2))

# ppareto is defined in the file pareto.q

# actual plotting code begins here
par(xaxs="i",yaxs="e",lwd=3)
# pareto survival function
# ppareto is defined in pareto.q
plot(g(x), 1-ppareto(x, a=1.1, cmin=0.25), type="l", 
	xlab="x", ylab="1-F(x)", axes=F, xlim=c(g(0.25),g(4)), log="x")
	
# survival function of a normal distribution conditional on 
# X > 0.25
lines(g(x), (1 - pnorm(x,0,0.3113))/(1 - pnorm(0.25,0,0.3113)), lty=3)

# survival function of an exponential distribution conditional on 
# X > 0.25
lines(g(x), (1 - pexp(x, scale=0.25/1.1))/(1 - pexp(0.25, scale=0.25/1.1)), lty=5)

axis(1, at=g(c(0.25,0.5,1,2,4)), labels=as.character(c(0.25,0.5,1,2,4)))
axis(2, at=seq(0,1,0.1), adj=1)

text(g(0.26),0.15,"Normal", adj=0)
text(g(1.1) ,0.1 ,"Exp"   , adj=0)
text(g(0.85) ,0.45,"Pareto", adj=0)

arrows(g(0.35),0.15, g(0.50), (1 - pnorm(0.50,0,0.3113))/(1 - pnorm(0.25,0,0.3113)), size=.1)
arrows(g(1.0 ),0.1 , g(0.70), (1 - pexp(0.70,scale=0.25/1.1))/(1 - pexp(0.25,scale=0.25/1.1)), size=.1)
arrows(g(0.8 ),0.45, g(0.75), 1-ppareto(0.75, a=1.1, cmin=0.25), size=.1)

box()
