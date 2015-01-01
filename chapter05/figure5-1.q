############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# figure5-1.q                                              #
#                                                          #
############################################################

# sequence of weights
w <- seq(0,1,0.01)

# thicker lines, internal axes, square plot, horizontal axis labels
par(lwd=3,xaxs="i",yaxs="i", pty="s", las=1)

# plot risk = w/4 versus return = .06 + .09w
plot(0.25*w, 0.06 + 0.09*w, type="l", axes=F, 
	xlab="", ylab="", xlim=c(0,0.25), ylim=c(0,0.2))

# mimic axes in the book
axis(1, at=seq(0,0.25,0.05))
axis(2, at=seq(0,0.20,0.02), adj=1)
box()
mtext("\"Risk\" = sigma_R = w/4", side=1, line=2)
mtext("\"Reward\" = E(R) = .06 + .09w", side=2, line=4)
